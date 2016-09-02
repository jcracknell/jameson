package jameson
package enc

import scala.annotation.tailrec
import java.io.Writer

class EncodingJValueWriter(ctx: JEncodingContext) extends JValueWriter with AutoCloseable {
  private var _path = ctx.path
  private var consumed = false
  private var closed = false

  def path: JPath = _path

  def writeBoolean(value: Boolean): Unit = {
    consume()
    if(value) ctx.writer.write("true") else ctx.writer.write("false")
  }

  def writeBoolean(bool: JBoolean): Unit = writeBoolean(bool.value)

  def writeNull(): Unit = {
    consume()
    ctx.writer.write("null")
  }

  def writeNumber(value: Double): Unit = {
    consume()

    EncodingJValueWriter.encodeNumber(value, ctx.writer)
  }

  def writeNumber(number: JNumber): Unit = writeNumber(number.value)

  def writeString(value: String): Unit = {
    consume()

    EncodingJValueWriter.encodeString(value, ctx.writer, ctx.options)
  }

  def writeString(str: JString): Unit = writeString(str.value)

  def writeString(loan: JStringWriter => Unit): Unit = {
    consume()

    ctx.writer.write('"')
    using(new EncodingJStringWriter(ctx))(loan)
    ctx.writer.write('"')
  }

  def writeArray(sizeHint: Int)(loan: JArrayWriter => Unit): Unit = {
    consume()

    val arrayStyle = ctx.options.arrayStyleAt(ctx.path, sizeHint)
    using(new EncodingJArrayWriter(ctx, arrayStyle)) { arrayWriter =>
      arrayStyle.writeStart(ctx.writer)
      loan(arrayWriter)
      arrayStyle.writeEnd(ctx.writer, arrayWriter.writeCount)
    }
  }

  def writeArray(arr: JArray): Unit =
    writeArray(arr.elements.length) { aw => arr.elements foreach { el => aw.write(el) } }

  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): Unit = {
    consume()

    val objectStyle = ctx.options.objectStyleAt(ctx.path, sizeHint)
    using(new EncodingJObjectWriter(ctx, objectStyle)) { objectWriter =>
      objectStyle.writeStart(ctx.writer)
      loan(objectWriter)
      objectStyle.writeEnd(ctx.writer, objectWriter.writeCount)
    }
  }

  def writeObject(obj: JObject): Unit =
    writeObject(obj.seq.length) { ow => obj.seq foreach { case (name, value) => ow.write(name, value) } }

  private def consume(): Unit = {
    if(consumed)
      throw new IllegalStateException("Attempt to access consumed JValueWriter.")
    if(closed)
      throw new IllegalStateException("Attempt to access closed JValueWriter.")

    consumed = true
  }

  /** Resets the writer to the open, unconsumed state. */
  def reset(p: JPath): this.type = {
    _path = p
    consumed = false
    closed = false
    this
  }

  def close(): Unit = {
    if(!consumed)
      throw new IllegalStateException("JValueWriter was not consumed.")

    closed = true
  }
}

object EncodingJValueWriter {
  def encodeNumber(value: Double, writer: java.io.Writer): Unit = {
    val intVal = value.toInt
    if(intVal.toDouble == value) writer.write(intVal.toString) else {
      val str = value.toString
      val strLen = str.length

      @tailrec def frac(i: Int, s: Int, e: Int): Unit = if(i == strLen) writer.write(str, s, e - s) else str.charAt(i) match {
        case '0' => frac(i + 1, s, e)
        case 'e' | 'E' =>
          if(i == e) writer.write(str, s, strLen - i) else {
            writer.write(str, s, e - s)
            writer.write(str, i, strLen - i)
          }
        case _ => frac(i + 1, s, i + 1)
      }

      // N.B. that a string representation of a double always contains a decimal point
      var i = 0
      while(str.charAt(i) != '.') i += 1

      writer.write(str, 0, i)
      frac(i + 1, i, i)
    }
  }

  def encodeString(str: String): String = encodeString(str, new Jameson.EncodingOptions())

  def encodeString(str: String, options: JEncodingOptions): String = {
    val sw = new java.io.StringWriter(128)
    encodeString(str, sw, options)
    sw.toString
  }

  def encodeString(str: String, writer: Writer): Unit =
    encodeString(str, writer, new Jameson.EncodingOptions())

  def encodeString(str: String, writer: Writer, options: JEncodingOptions): Unit = {
    writer.write('"')
    encodeStringFragment(str, writer, options)
    writer.write('"')
  }

  def encodeStringFragment(c: Char, writer: Writer): Unit =
    encodeStringFragment(c, writer, new Jameson.EncodingOptions())

  def encodeStringFragment(c: Char, writer: Writer, options: JEncodingOptions): Unit = {
    val esc = escape(c, options)
    if(esc != null) writer.write(esc) else writer.write(c.toInt)
  }

  def encodeStringFragment(str: String, writer: Writer): Unit =
    encodeStringFragment(str, writer, new Jameson.EncodingOptions())

  def encodeStringFragment(str: String, writer: Writer, options: JEncodingOptions): Unit =
    encodeStringFragment(str, 0, str.length, writer, options)

  def encodeStringFragment(str: String, offset: Int, length: Int, writer: Writer): Unit =
    encodeStringFragment(str, offset, length, writer, new Jameson.EncodingOptions())

  def encodeStringFragment(chars: Array[Char], offset: Int, length: Int, writer: Writer, options: JEncodingOptions): Unit = {
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

  def encodeStringFragment(str: String, offset: Int, length: Int, writer: Writer, options: JEncodingOptions): Unit = {
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

  private def escape(c: Char, options: JEncodingOptions): Array[Char] = c match {
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

  def shouldEscape(c: Char, options: JEncodingOptions): Boolean = c match {
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
}
