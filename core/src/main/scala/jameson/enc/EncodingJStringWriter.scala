package jameson
package enc

class EncodingJStringWriter(ctx: JEncodingContext) extends JStringWriter {
  val path = ctx.path
  private var closed = false

  override def write(c: Int): Unit = {
    guard()

    EncodingJValueWriter.encodeStringFragment(c.toChar, ctx.writer, ctx.options)
  }

  override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
    guard()

    EncodingJValueWriter.encodeStringFragment(chars, offset, length, ctx.writer, ctx.options)
  }

  override def flush(): Unit = {
    guard()

    ctx.writer.flush()
  }

  override def close(): Unit = {
    closed = true
  }

  private def guard(): Unit = {
    if(closed)
      throw new IllegalStateException("Attempted to access closed JStringWriter.")
  }
}
