package jameson.enc

import java.io.{StringWriter, Writer}

object JStringEncoder {
  def encode(str: String): String = encode(str, defaultOptions)

  def encode(str: String, options: Options): String = {
    val sw = new StringWriter(128)
    encode(str, sw, options)
    sw.toString
  }

  def encode(str: String, writer: Writer): Unit =
    encode(str, writer, defaultOptions)

  def encode(str: String, writer: Writer, options: Options): Unit = {
    writer.write('"')
    encodeFragment(str, writer, options)
    writer.write('"')
  }

  def encodeFragment(c: Char, writer: Writer): Unit =
    encodeFragment(c, writer, defaultOptions)

  def encodeFragment(c: Char, writer: Writer, options: Options): Unit = {
    val esc = escape(c, options)
    if(esc != null) writer.write(esc) else writer.write(c)
  }

  def encodeFragment(str: String, writer: Writer): Unit =
    encodeFragment(str, writer, defaultOptions)

  def encodeFragment(str: String, writer: Writer, options: Options): Unit =
    encodeFragment(str, 0, str.length, writer, options)

  def encodeFragment(str: String, offset: Int, length: Int, writer: Writer): Unit =
    encodeFragment(str, offset, length, writer, defaultOptions)

  def encodeFragment(chars: Array[Char], offset: Int, length: Int, writer: Writer, options: Options): Unit = {
    val end = offset + length
    var rp = offset
    var wp = offset

    while(rp < end) {
      val esc = escape(chars(rp), options)
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

  def encodeFragment(str: String, offset: Int, length: Int, writer: Writer, options: Options): Unit = {
    val end = offset + length
    var rp = offset
    var wp = offset

    while(rp < end) {
      val esc = escape(str.charAt(rp), options)
      if(esc != null) {
        if(wp < rp) {
          writer.write(str, wp, rp - wp)
        }
        wp = rp + 1

        writer.write(esc, 0, esc.length)
      }

      rp += 1
    }

    if(wp < rp) {
      writer.write(str, wp, rp - wp)
    }
  }

  private def escape(c: Char, options: Options): Array[Char] = c match {
    case '"'  => Array('\\', '"')
    case '\\' => Array('\\', '\\')
    case '\b' => Array('\\', 'b')
    case '\f' => Array('\\', 'f')
    case '\n' => Array('\\', 'n')
    case '\r' => Array('\\', 'r')
    case '\t' => Array('\\', 't')
    case '/' if options.escapeSlashes => Array('\\', '/')
    case c => if(!shouldEscape(c, options)) null else {
      val i = c.toInt
      Array('\\', 'u', hex(i >>> 12), hex(i >>> 8), hex(i >>> 4), hex(i))
    }
  }

  def shouldEscape(c: Char, options: Options): Boolean = c match {
    case '\'' if options.escapeQuotes => true
    case c if options.escapeNonASCII && (c > '~' || ' ' > c) => true
    case c => Character.isISOControl(c)
  }

  private def hex(i: Int): Char = i & 0xF match {
    case 0x0 => '0'  case 0x4 => '4'  case 0x8 => '8'  case 0xC => 'c'
    case 0x1 => '1'  case 0x5 => '5'  case 0x9 => '9'  case 0xD => 'd'
    case 0x2 => '2'  case 0x6 => '6'  case 0xA => 'a'  case 0xE => 'e'
    case 0x3 => '3'  case 0x7 => '7'  case 0xB => 'b'  case 0xF => 'f'
    case _ => throw new Exception("Invalid nibble")
  }

  object defaultOptions extends Options {
    def escapeNonASCII: Boolean = false
    def escapeSlashes: Boolean = false
    def escapeQuotes: Boolean = false
  }

  trait Options {
    /** Indicates whether non lower-ASCII characters should always be escaped. */
    def escapeNonASCII: Boolean

    /** Indicates whether single quotes should be escaped in addition to double quotes. */
    def escapeQuotes: Boolean

    /** Indicates whether forward slashes should be escaped in addition to backslashes.
      * Useful when embedding JSON inside of `<script>` tags.
      */
    def escapeSlashes: Boolean
  }
}
