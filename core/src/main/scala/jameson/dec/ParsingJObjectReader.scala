package jameson
package dec

import scala.annotation.tailrec

import JParser._

class ParsingJObjectReader(ctx: JParsingContext) extends BaseJObjectReader {
  val path: JPath = ctx.path

  protected def consume(f: (String, JReader) => Unit): Unit = {
    // Initialize a buffer and StringBuilder we can reuse for every property name
    val nameBuffer = Array.ofDim[Char](64)
    val nameBuilder = new java.lang.StringBuilder

    @tailrec def readName(nameReader: ParsingJStringReader): String = {
      val n = nameReader.read(nameBuffer, 0, nameBuffer.length)
      if(n <= 0) nameBuilder.toString else {
        nameBuilder.append(nameBuffer, 0, n)
        readName(nameReader)
      }
    }

    ctx.require('{')

    if(whitespace(ctx) && !ctx.ahead('}')) do {
      if(!ctx.consume('"'))
        ctx.error("Expected property name")

      val name = readName(new ParsingJStringReader(ctx))
      nameBuilder.setLength(0)

      whitespace(ctx); ctx.require(':'); whitespace(ctx)

      ctx.pathDown(name)
      value(ctx) { valueReader =>
        f(name, valueReader)
      }
      ctx.pathUp()
    } while(whitespace(ctx) && ctx.consume(',') && whitespace(ctx))

    ctx.require('}')
  }
}
