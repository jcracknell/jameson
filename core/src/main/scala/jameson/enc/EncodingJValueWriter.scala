package jameson
package enc

class EncodingJValueWriter(ctx: EncodingContext) extends JValueWriter with AutoCloseable {
  private var consumed = false
  private var closed = false

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
    ctx.writer.write(value.toString)
  }

  def writeNumber(number: JNumber): Unit = writeNumber(number.value)

  def writeString(value: String): Unit = {
    consume()

    JStringEncoder.encode(value, ctx.writer, ctx.options)
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

    arrayStyle.writeStart(ctx.writer)
    using(new EncodingJArrayWriter(ctx, arrayStyle))(loan)
    arrayStyle.writeEnd(ctx.writer)
  }

  def writeArray(arr: JArray): Unit =
    writeArray(arr.elements.length) { aw => arr.elements foreach { el => aw.write(el) } }

  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): Unit = {
    consume()

    val objectStyle = ctx.options.objectStyleAt(ctx.path, sizeHint)

    objectStyle.writeStart(ctx.writer)
    using(new EncodingJObjectWriter(ctx, objectStyle))(loan)
    objectStyle.writeEnd(ctx.writer)
  }

  def writeObject(obj: JObject): Unit =
    writeObject(obj.seq.length) { ow => obj.seq foreach { case (name, value) => ow.write(name, value) } }

  private def consume(): Unit = {
    if(consumed)
      throw new UnsupportedOperationException("Attempt to access consumed JValueWriter.")
    if(closed)
      throw new UnsupportedOperationException("Attempt to access closed JValueWriter.")

    consumed = true
  }

  /** Resets the writer to the open, unconsumed state. */
  def reset(): EncodingJValueWriter = {
    consumed = false
    closed = false
    this
  }

  def close(): Unit = {
    if(!consumed)
      throw new UnsupportedOperationException("JValueWriter was not consumed.")

    closed = true
  }
}
