package jameson
package enc

class StreamingJStringWriter(ctx: EncodingContext) extends JStringWriter {
  private var closed = false

  override def write(c: Int): Unit = {
    guard()
    JStringEncoder.encodeFragment(c.toChar, ctx.writer, ctx.options)
  }

  override def write(chars: Array[Char], offset: Int, length: Int): Unit = {
    guard()
    JStringEncoder.encodeFragment(chars, offset, length, ctx.writer, ctx.options)
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
      throw new UnsupportedOperationException("Attempted to access closed JStringWriter.")
  }
}
