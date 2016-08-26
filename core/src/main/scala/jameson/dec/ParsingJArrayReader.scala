package jameson
package dec

import Parser._

class ParsingJArrayReader(ctx: ParsingContext) extends BaseJArrayReader {
  protected def consume(f: (Int, JReader) => Unit): Unit = {
    var i = 0

    ctx.require('[')

    if(whitespace(ctx) && !ctx.ahead(']')) do {
      ctx.path(i)
      value(ctx) { valueReader =>
        f(i, valueReader)
      }
      ctx.pathUp()

      i += 1
    } while(whitespace(ctx) && ctx.consume(',') && whitespace(ctx))

    ctx.require(']')
  }
}
