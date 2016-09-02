package jameson
package enc

import scala.annotation.tailrec

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

    EncodingJStringWriter.encode(value, ctx.writer, ctx.options)
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

      // N.B. that a string representation of a double always contains a decimal point
      @tailrec def int(i: Int): Unit = str.charAt(i) match {
        case '.' =>
          writer.write(str, 0, i)
          frac(i + 1, i, i)
        case _ => int(i + 1)
      }

      @tailrec def frac(i: Int, s: Int, e: Int): Unit = if(i == strLen) writer.write(str, s, e - s) else str.charAt(i) match {
        case '0' => frac(i + 1, s, e)
        case 'e' | 'E' =>
          if(i == e) writer.write(str, s, strLen - i) else {
            writer.write(str, s, e - s)
            writer.write(str, i, strLen - i)
          }
        case _ => frac(i + 1, s, i + 1)
      }

      int(0)
    }
  }
}
