package jameson
package enc

class EncodingJObjectWriter(ctx: EncodingContext, objectStyle: JObjectStyle) extends JObjectWriter with AutoCloseable {
  private val path = ctx.path
  private val valueWriter = new EncodingJValueWriter(ctx)

  private var i = 0
  private var closed = false
  private var controlPassed = false

  def write[A](name: String, a: A)(implicit encoder: JEncoder[A]): JObjectWriter = {
    guard()

    down(name)
    using(valueWriter.reset()) { _ =>
      encoder.encode(a, valueWriter)
    }
    up()
  }

  def writeArray(name: String, sizeHint: Int)(loan: JArrayWriter => Unit): JObjectWriter = {
    guard()

    down(name)
    valueWriter.reset().writeArray(sizeHint)(loan)
    up()
  }

  def writeObject(name: String, sizeHint: Int)(loan: JObjectWriter => Unit): JObjectWriter = {
    guard()

    down(name)
    valueWriter.reset().writeObject(sizeHint)(loan)
    up()
  }

  def writeString(name: String)(loan: JStringWriter => Unit): JObjectWriter = {
    guard()

    down(name)
    valueWriter.reset().writeString(loan)
    up()
  }

  private def down(name: String): Unit = {
    if(i != 0) objectStyle.writeSeparator(ctx.writer)
    objectStyle.writeIndent(ctx.writer)

    i += 1

    valueWriter.reset().writeString(name)
    objectStyle.writeAssign(ctx.writer)

    ctx.pathDown(name)
    controlPassed = true
  }

  private def up(): JObjectWriter = {
    ctx.pathUp()
    controlPassed = false
    this
  }

  private def guard(): Unit = {
    if(closed)
      throw new UnsupportedOperationException("Attempted to access closed JObjectWriter.")
    if(controlPassed)
      throw new UnsupportedOperationException(s"Out of scope access for JObjectWriter(${this.path}) from ${ctx.path}")
  }

  def close(): Unit = {
    closed = true
  }

  override def toString: String = s"JObjectWriter($path)"
}
