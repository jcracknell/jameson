package jameson
package enc

class EncodingJObjectWriter(ctx: JEncodingContext, objectStyle: JObjectStyle) extends JObjectWriter with AutoCloseable {
  val path = ctx.path
  private val valueWriter = new EncodingJValueWriter(ctx)

  private var i = 0
  private var closed = false
  private var controlPassed = false

  def writeCount: Int = {
    guard()
    i
  }

  def write[A](name: String, a: A)(implicit encoder: JEncoder[A]): JObjectWriter = {
    guard()

    down(name)
    using(valueWriter.reset(ctx.path)) { _ =>
      encoder.encode(a, valueWriter)
    }
    up()
  }

  def writeArray(name: String, sizeHint: Int)(loan: JArrayWriter => Unit): JObjectWriter = {
    guard()

    down(name)
    valueWriter.reset(ctx.path).writeArray(sizeHint)(loan)
    up()
  }

  def writeObject(name: String, sizeHint: Int)(loan: JObjectWriter => Unit): JObjectWriter = {
    guard()

    down(name)
    valueWriter.reset(ctx.path).writeObject(sizeHint)(loan)
    up()
  }

  def writeString(name: String)(loan: JStringWriter => Unit): JObjectWriter = {
    guard()

    down(name)
    valueWriter.reset(ctx.path).writeString(loan)
    up()
  }

  private def down(name: String): Unit = {
    if(i != 0) objectStyle.writeSeparator(ctx.writer)
    objectStyle.writeIndent(ctx.writer)

    i += 1

    EncodingJValueWriter.encodeString(name, ctx.writer, ctx.options)
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
      throw new IllegalStateException("Attempted to access closed JObjectWriter.")
    if(controlPassed)
      throw new IllegalStateException(s"Out of scope access for JObjectWriter(${this.path}) from ${ctx.path}")
  }

  def close(): Unit = {
    closed = true
  }

  override def toString: String = s"JObjectWriter($path)"
}
