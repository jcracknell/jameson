package jameson
package dec

import scala.annotation.tailrec

import JParser._

class ParsingJObjectReader(ctx: JParsingContext) extends BaseJObjectReader {
  val path: JPath = ctx.path

  protected def consume(f: (String, JReader) => Unit): Unit = {
    // Initialize a StringBuilder we can reuse for every property name
    val nameBuilder = new java.lang.StringBuilder

    @tailrec def readName(nameReader: ParsingJStringReader): String = {
      val n = nameReader.read(ctx.charBuffer, 0, ctx.charBuffer.length)
      if(n <= 0) nameBuilder.toString else {
        nameBuilder.append(ctx.charBuffer, 0, n)
        readName(nameReader)
      }
    }

    ctx.require('{')

    if(whitespace(ctx) && !ctx.ahead('}')) do {
      val quote = ctx.peek() match {
        case DOUBLE_QUOTE => DOUBLE_QUOTE
        case SINGLE_QUOTE if ctx.options.allowSingleQuotes => SINGLE_QUOTE
        case _ => ctx.error("Expected property name")
      }

      ctx.drop(1)
      val name = readName(new ParsingJStringReader(ctx, quote))
      nameBuilder.setLength(0)

      whitespace(ctx); ctx.require(':'); whitespace(ctx)

      ctx.path = ctx.path / name
      value(ctx) { valueReader =>
        f(name, valueReader)
      }
      ctx.path = ctx.path.parent
    } while(whitespace(ctx) && ctx.consume(',') && whitespace(ctx))

    ctx.require('}')
  }
}
