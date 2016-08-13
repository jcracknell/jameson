package jameson

import java.io.Writer

sealed trait JPath {
  def isBase: Boolean
  def parent: JPath
  def base: JPath.Base = if(isBase) this.asInstanceOf[JPath.Base] else parent.base

  def resolve(ctx: JLookup): JLookup

  def /(name: String): JPath = new JPath.Property(this, name)
  def /(index: Int): JPath = new JPath.Index(this, index)

  def depth: Int = if(isBase) 0 else 1 + parent.depth

  /** The string describing the path relative to its base. */
  def pathString: String = {
    val sw = new java.io.StringWriter
    pathStringTo(sw)
    sw.toString
  }

  protected def pathStringTo(writer: Writer): Unit

  override def toString: String = {
    val sw = new java.io.StringWriter
    val baseString = base.baseString
    if(baseString.length > 0) {
      sw.write(baseString)
      sw.write("#")
    }
    pathStringTo(sw)
    sw.toString
  }
}



object JPath {
  def relative: JPath = RelativeBase
  def stream: JPath = UnknownBase
  def value(v: JValue): JPath = new ValueBase(v)
  def uri(uri: java.net.URI): JPath = new UriBase(uri)
  def uri(str: String): JPath = uri(java.net.URI.create(str))
  def file(file: java.io.File): JPath = new FileBase(file)
  def file(str: String): JPath = file(new java.io.File(str))

  sealed trait Base extends JPath {
    def isBase: Boolean = true
    def parent: JPath = throw new UnsupportedOperationException()
    def resolve(ctx: JLookup): JLookup = ctx

    def baseString: String

    protected def pathStringTo(writer: Writer): Unit = writer.write(".")
  }

  case class ValueBase(value: JValue) extends Base {
    def baseString: String = s"${value.getClass.getName}@${java.lang.System.identityHashCode(value)}"
  }
  case class FileBase(file: java.io.File) extends Base {
    def baseString: String = file.getPath
  }

  case class UriBase(uri: java.net.URI) extends Base {
    def baseString: String = uri.toString
  }

  case object UnknownBase extends Base {
    def baseString: String = "<?>"
  }

  case object RelativeBase extends Base {
    def baseString: String = ""
  }

  case class Property(parent: JPath, name: String) extends JPath {
    def isBase: Boolean = false
    def resolve(ctx: JLookup): JLookup = parent.resolve(ctx)(name)

    protected def pathStringTo(writer: Writer): Unit = {
      parent.pathStringTo(writer)
      writer.write("[")
      JString.encode(name, writer)
      writer.write("]")
    }
  }

  case class Index(parent: JPath, index: Int) extends JPath {
    if(index < 0) throw new IndexOutOfBoundsException(index.toString)

    def isBase: Boolean = false
    def resolve(ctx: JLookup): JLookup = parent.resolve(ctx)(index)

    protected def pathStringTo(writer: Writer): Unit = {
      parent.pathStringTo(writer)
      writer.write("[")
      writer.write(index.toString)
      writer.write("]")
    }
  }
}
