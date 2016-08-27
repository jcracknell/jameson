package jameson
package dec

import scala.annotation.tailrec

object JParser {
  val ASTERIX              = 0x2A
  val BACKSLASH            = 0x5C
  val COLON                = 0x3A
  val COMMA                = 0x2C
  val DOUBLE_QUOTE         = 0x22
  val FORWARD_SLASH        = 0x2F
  val LEFT_CURLY_BRACKET   = 0x7B
  val LEFT_SQUARE_BRACKET  = 0x5B
  val LOWERCASE_E          = 0x65
  val LOWERCASE_F          = 0x66
  val LOWERCASE_N          = 0x6E
  val LOWERCASE_T          = 0x74
  val LOWERCASE_U          = 0x75
  val MINUS_SIGN           = 0x2D
  val PERIOD               = 0x2E
  val PLUS_SIGN            = 0x2B
  val RIGHT_CURLY_BRACKET  = 0x7D
  val RIGHT_SQUARE_BRACKET = 0x5D
  val SINGLE_QUOTE         = 0x27
  val UPPERCASE_E          = 0x45
  val ZERO                 = 0x30

  // Whitespace characters
  val TAB = 0x09
  val LF  = 0x0A
  val CR  = 0x0D
  val SP  = 0x20

  def parse[A](ctx: JParsingContext)(handler: JReader => A): A = {
    whitespace(ctx)
    val result = value(ctx)(handler)
    whitespace(ctx)

    if(!ctx.atEOF)
      ctx.error("Expected end of input")

    result
  }

  def value[A](ctx: JParsingContext)(handler: JReader => A): A = ctx.peek() match {
    case LOWERCASE_F => ctx.require("false"); handler(JFalse)
    case LOWERCASE_T => ctx.require("true"); handler(JTrue)
    case LOWERCASE_N => ctx.require("null"); handler(JNull)
    case LEFT_SQUARE_BRACKET => using(new ParsingJArrayReader(ctx))(handler)
    case LEFT_CURLY_BRACKET => using(new ParsingJObjectReader(ctx))(handler)

    case DOUBLE_QUOTE =>
      ctx.drop(1)
      using(new ParsingJStringReader(ctx, DOUBLE_QUOTE))(handler)

    case 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37 | 0x38 | 0x39 | MINUS_SIGN =>
      handler(number(ctx))

    case SINGLE_QUOTE if ctx.options.allowSingleQuotes =>
      ctx.drop(1)
      using(new ParsingJStringReader(ctx, SINGLE_QUOTE))(handler)

    case c if c < 0 => ctx.error("Unexpected end of input")
    case _          => ctx.error("JSON syntax error")
  }

  def number(ctx: JParsingContext): JNumber = {
    // -?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)
    val sb = new java.lang.StringBuilder(16)

    def sign(): Unit = if(ctx.consume('-')) sb.append('-')

    def int(): Unit = {
      val c = ctx.peek()
      if(!isDigit(c))
        ctx.error("Expected integer part in number literal")

      // If this was a non-zero digit, we can handle this as digits
      if(c != ZERO) digits() else {
        sb.append(c.toChar)
        ctx.drop(1)
      }
    }

    def frac(): Unit = if(ctx.consume('.')) {
      sb.append('.')
      digits()
    }

    def exp(): Unit = if(e()) digits()

    def digits(): Unit = {
      val c = ctx.peek()
      if(!isDigit(c))
        ctx.error("Expected digits in number literal")

      sb.append(c.toChar)
      ctx.drop(1)

      @tailrec def loop(): Unit = {
        val c = ctx.peek()
        if(isDigit(c)) {
          sb.append(c.toChar)
          ctx.drop(1)
          loop()
        }
      }
      loop()
    }

    def e(): Boolean = ctx.peek() match {
      case c @ (LOWERCASE_E | UPPERCASE_E) =>
        sb.append(c.toChar)
        ctx.drop(1)
        ctx.peek() match {
          case c @ (MINUS_SIGN | PLUS_SIGN) =>
            sb.append(c.toChar)
            ctx.drop(1)
          case _ =>
        }
        true
      case _ => false
    }

    sign(); int(); frac(); exp()

    try new JNumber(java.lang.Double.parseDouble(sb.toString)) catch {
      case _: Throwable =>
        ctx.error("Invalid number literal")
    }
  }

  /** Consumes any whitespace characters in the input stream. Returns true as a convenience. */
  def whitespace(ctx: JParsingContext): Boolean = {
    def ws(): Unit = {
      while(isWhitespace(ctx.peek()))
        ctx.drop(1)
    }

    def comment(): Boolean = if(!ctx.ahead('/')) false else ctx.peek(1) match {
      case FORWARD_SLASH =>
        ctx.drop(2)
        while(!ctx.atEOF && !ctx.consume('\n'))
          ctx.drop(1)
        true

      case ASTERIX =>
        ctx.drop(2)
        while(!ctx.atEOF && !ctx.ahead("*/"))
          ctx.drop(1)
        ctx.require("*/")
        true

      case _ => false
    }

    if(ctx.options.allowComments) {
      do ws() while(comment())
    } else {
      ws()
    }

    true
  }

  def isDigit(c: Int): Boolean = c match {
    case 0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37 | 0x38 | 0x39 => true
    case _ => false
  }

  def isWhitespace(c: Int): Boolean = c match {
    case SP | TAB | CR | LF => true
    case _ => false
  }
}
