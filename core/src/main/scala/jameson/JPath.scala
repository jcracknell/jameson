package jameson

import java.io.Writer

sealed trait JPath {
  def isBase: Boolean
  def parent: JPath

  def resolve(ctx: JLookup): JLookup

  def /(name: String): JPath = new JPath.Property(this, name)
  def /(index: Int): JPath = new JPath.Index(this, index)

  def depth: Int = if(isBase) 0 else 1 + parent.depth

  protected def renderTo(writer: Writer): Unit

  override def toString: String = {
    val sw = new java.io.StringWriter
    renderTo(sw)
    sw.toString
  }
}

object JPath extends JPath {
  def isBase: Boolean = true
  def parent: JPath = throw new UnsupportedOperationException()
  def resolve(ctx: JLookup): JLookup = ctx

  protected def renderTo(writer: Writer): Unit = writer.write(".")

  override def hashCode(): Int = "JPath".hashCode

  override def toString: String = "."

  case class Property(parent: JPath, name: String) extends JPath {
    def isBase: Boolean = false
    def resolve(ctx: JLookup): JLookup = parent.resolve(ctx)(name)

    protected def renderTo(writer: Writer): Unit = {
      parent.renderTo(writer)
      writer.write("[")
      JString.encode(name, writer)
      writer.write("]")
    }
  }

  case class Index(parent: JPath, index: Int) extends JPath {
    if(index < 0) throw new IndexOutOfBoundsException(index.toString)

    def isBase: Boolean = false
    def resolve(ctx: JLookup): JLookup = parent.resolve(ctx)(index)

    protected def renderTo(writer: Writer): Unit = {
      parent.renderTo(writer)
      writer.write("[")
      writer.write(index.toString)
      writer.write("]")
    }
  }
}
