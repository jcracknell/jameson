package jameson
package dec

import jameson.util.IOUtil

import scala.annotation.tailrec
import JParser._

/** [[java.io.Reader]] implementation which decodes a JSON string from an underlying
  * [[java.io.Reader]]. Reading ends when a closing U+0022 QUOTATION MARK character
  * is encountered. Note that the leading quotation mark should have been consumed
  * from the reader.
  */
class ParsingJStringReader(ctx: JParsingContext, quote: Int) extends JStringReader {
  if(!(quote == DOUBLE_QUOTE || quote == SINGLE_QUOTE && ctx.options.allowSingleQuotes))
    throw new IllegalArgumentException("Invalid quote.")

  val path: JPath = ctx.path
  private var ended = false
  private var closed = false

  override def read(chars: Array[Char], offset: Int, length: Int): Int = {
    assertOpen()

    val end = offset + length
    @tailrec def loop(i: Int): Int = if(i >= end) i - offset else {
      val c = read()
      if(c < 0) i - offset else {
        chars(i) = c.toChar
        loop(i + 1)
      }
    }
    loop(offset)
  }

  override def read(): Int = {
    assertOpen()

    if(ended) -1 else {
      val c = ctx.peek()

      if(c == BACKSLASH) {
        readEscape()
      } else if(c == quote) {
        ctx.drop(1)
        ended = true
        -1
      } else {
        if(Character.isISOControl(c)) ctx.error("Invalid character in JSON string")
        if(c < 0) ctx.error("Unterminated JSON string")

        ctx.drop(1)
        c
      }
    }
  }

  private def readEscape(): Int = ctx.peek(1) match {
    case DOUBLE_QUOTE => ctx.drop(2); DOUBLE_QUOTE
    case BACKSLASH => ctx.drop(2); BACKSLASH
    case FORWARD_SLASH => ctx.drop(2); FORWARD_SLASH
    case 0x62 => ctx.drop(2); 0x08 // '\b'
    case 0x66 => ctx.drop(2); 0x0C // '\f'
    case 0x6E => ctx.drop(2); 0x0A // '\n'
    case 0x72 => ctx.drop(2); 0x0D // '\r'
    case 0x74 => ctx.drop(2); 0x09 // '\t'
    case 0x75 => readUnicodeEscape()
    case SINGLE_QUOTE if ctx.options.allowSingleQuotes => ctx.drop(2); SINGLE_QUOTE
    case _ => ctx.error("Invalid escape sequence in JSON string")
  }

  private def readUnicodeEscape(): Int = {
    def d(c: Int): Int = c match {
      case 0x30        => 0x0
      case 0x31        => 0x1
      case 0x32        => 0x2
      case 0x33        => 0x3
      case 0x34        => 0x4
      case 0x35        => 0x5
      case 0x36        => 0x6
      case 0x37        => 0x7
      case 0x38        => 0x8
      case 0x39        => 0x9
      case 0x41 | 0x61 => 0xA
      case 0x42 | 0x62 => 0xB
      case 0x43 | 0x63 => 0xC
      case 0x44 | 0x64 => 0xD
      case 0x45 | 0x65 => 0xE
      case 0x46 | 0x66 => 0xF
      case c if c < 0 => ctx.error("Unexpected end of input in JSON string.")
      case _ => ctx.error("Invalid unicode escape.")
    }

    val c = d(ctx.peek(2)) << 12 | d(ctx.peek(3)) << 8 | d(ctx.peek(4)) << 4 | d(ctx.peek(5))
    ctx.drop(6)
    c
  }

  def discard(): Unit = {
    assertOpen()

    while(read() >= 0) {}
  }

  def copy(): JString = {
    assertOpen()

    val sw = new java.io.StringWriter(128)
    IOUtil.copy(this, sw, 128)
    new JString(sw.toString)
  }

  /** Closes the reader, preventing any further access to public methods.
    * Throws an exception in the event that the reader has not been fully consumed.
    */
  override def close(): Unit = {
    if(!ended) throw new IllegalStateException("Attempt to close unconsumed JStringReader.")

    closed = true
  }

  @inline private def assertOpen(): Unit =
    if(closed) throw new IllegalStateException("Attempt to access closed JStringReader.")
}
