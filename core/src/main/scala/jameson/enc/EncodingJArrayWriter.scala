package jameson
package enc

class EncodingJArrayWriter(ctx: JEncodingContext, arrayStyle: JArrayStyle) extends JArrayWriter with AutoCloseable {
  private val path = ctx.path
  private val valueWriter = new EncodingJValueWriter(ctx)

  private var i = 0
  private var closed = false
  private var controlPassed = false

  def writeCount: Int = {
    guard()
    i
  }

  def write[A](a: A)(implicit encoder: JEncoder[A]): JArrayWriter = {
    guard()

    down()
    using(valueWriter.reset()) { _ =>
      encoder.encode(a, valueWriter)
    }
    up()
  }

  def writeArray(sizeHint: Int)(loan: (JArrayWriter) => Unit): JArrayWriter = {
    guard()

    down()
    valueWriter.reset().writeArray(sizeHint)(loan)
    up()
  }

  def writeObject(sizeHint: Int)(loan: (JObjectWriter) => Unit): JArrayWriter = {
    guard()

    down()
    valueWriter.reset().writeObject(sizeHint)(loan)
    up()
  }

  def writeString(loan: (JStringWriter) => Unit): JArrayWriter = {
    guard()

    down()
    valueWriter.reset().writeString(loan)
    up()
  }

  private def down(): Unit = {
    if(i != 0) arrayStyle.writeSeparator(ctx.writer)
    arrayStyle.writeIndent(ctx.writer)

    ctx.pathDown(i)
    controlPassed = true
  }

  private def up(): JArrayWriter = {
    ctx.pathUp()
    controlPassed = false
    i += 1
    this
  }

  private def guard(): Unit = {
    if(closed)
      throw new IllegalStateException("Attempt to access closed JArrayWriter.")
    if(controlPassed)
      throw new IllegalStateException(s"Out of scope access for JArrayWriter(${this.path}) from ${ctx.path}.")
  }

  def close(): Unit = {
    closed = true
  }

  override def toString: String = s"JArrayWriter($path)"
}
