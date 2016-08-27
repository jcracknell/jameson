package jameson
package dec

import JParser._

class ParsingJArrayReader(ctx: JParsingContext) extends BaseJArrayReader {
  val path: JPath = ctx.path

  protected def consume(f: (Int, JReader) => Unit): Unit = {
    var i = 0

    ctx.require('[')

    if(whitespace(ctx) && !ctx.ahead(']')) do {
      ctx.path = ctx.path / i
      value(ctx) { valueReader =>
        f(i, valueReader)
      }
      ctx.path = ctx.path.parent

      i += 1
    } while(whitespace(ctx) && ctx.consume(',') && whitespace(ctx))

    ctx.require(']')
  }
}
