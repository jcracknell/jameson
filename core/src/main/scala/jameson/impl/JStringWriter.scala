package jameson
package impl

import java.io.Writer

/** [[java.io.Writer]] implementation which encodes JSON string content to an underlying
  * [[java.io.Writer]]. Note that the output to the writer does not include quotation
  * marks, and as such this class can be used to encode string fragments.
  */
class JStringWriter(writer: Writer, options: JStringWriter.Options = JStringWriter.defaultOptions) extends Writer {
  private var closed = false

  override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
    if(closed)
      throw new UnsupportedOperationException(s"Attempt to access closed ${getClass.getName}.")

    val end = offset + length
    var rp = offset
    var wp = offset

    while(rp < end) {
      val c = chars(rp)
      val esc = escape(c)
      if(esc != null) {
        if(wp < rp) {
          writer.write(chars, wp, rp - wp)
        }
        wp = rp + 1

        writer.write(esc, 0, esc.length)
      }
      rp += 1
    }

    if(wp < rp) {
      writer.write(chars, wp, rp - wp)
    }
  }

  override def write(c: Int): Unit = {
    if(closed)
      throw new UnsupportedOperationException(s"Attempt to access closed ${getClass.getName}.")

    val esc = escape(c.toChar)
    if(esc == null) writer.write(c) else writer.write(esc)
  }

  private def escape(c: Char): Array[Char] = c match {
    case '"'  => Array('\\',  '"')
    case '\\' => Array('\\', '\\')
    //case '/'  => Array('\\', '/')
    case '\b' => Array('\\',  'b')
    case '\f' => Array('\\',  'f')
    case '\n' => Array('\\',  'n')
    case '\r' => Array('\\',  'r')
    case '\t' => Array('\\',  't')
    case _ => if(shouldEscape(c)) unicodeEscape(c) else null
  }

  private def shouldEscape(c: Char): Boolean =
    Character.isISOControl(c) || options.escapeNonASCII && (c > '~' || ' ' > c)

  private def unicodeEscape(c: Char): Array[Char] = {
    // N.B. that it is best to encode to lowercase, as these characters occur more
    // frequently and will compress better.
    def hex(i: Int): Char = i & 0xF match {
      case 0x0 => '0'  case 0x4 => '4'  case 0x8 => '8'  case 0xC => 'c'
      case 0x1 => '1'  case 0x5 => '5'  case 0x9 => '9'  case 0xD => 'd'
      case 0x2 => '2'  case 0x6 => '6'  case 0xA => 'a'  case 0xE => 'e'
      case 0x3 => '3'  case 0x7 => '7'  case 0xB => 'b'  case 0xF => 'f'
      case _ => throw new Exception("Invalid nibble.")
    }

    val i = c.toInt
    Array('\\', 'u', hex(i >>> 12), hex(i >>> 8), hex(i >>> 4), hex(i))
  }

  override def flush(): Unit = {
    if(closed)
      throw new UnsupportedOperationException(s"Attempt to access closed ${getClass.getName}.")

    writer.flush()
  }

  override def close(): Unit = {
    closed = true
  }

}

object JStringWriter {
  trait Options {
    /** Indicates whether or not all non-ASCII characters should be escaped. */
    def escapeNonASCII: Boolean
  }

  object defaultOptions extends Options {
    def escapeNonASCII: Boolean = false
  }
}
