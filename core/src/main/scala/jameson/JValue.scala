package jameson

import jameson.impl.StreamingJStringWriter
import jameson.util.IOUtil
import java.io.{Reader, Writer}

import scala.language.implicitConversions

sealed trait JLookup {
  def isUndefined: Boolean

  /** Attempts to retrieve the value of the property with the specified name. Returns
    * [[JUndefined]] if there is no such property or the subject is not an object.
    */
  def apply(name: String): JLookup

  /** Attempts to retrieve the array element at the specified index. Returns
    * [[JUndefined]] if there is no such element or the subject is not an array.
    */
  def apply(index: Int): JLookup

  /** Attempts to retrieve the value at the specified path relative to the subject. */
  def apply(path: JPath): JLookup = path.resolve(this)

  /** Attempts to retrieve the value of the property with the specified name. Returns
    * [[JUndefined]] if there is no such property or the subject is not an object.
    */
  def /(name: String): JLookup = apply(name)

  /** Attempts to retrieve the value of the property with the specified name. Returns
    * [[JUndefined]] if there is no such property or the subject is not an object.
    */
  def /(index: Int): JLookup = apply(index)
}

case object JUndefined extends JLookup {
  def isUndefined: Boolean = true
  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = JUndefined
}

sealed trait JValue extends JLookup {
  def isUndefined: Boolean = false
}

sealed trait JScalar extends JValue {
  /** Get the native representation of this scalar JSON value. */
  def value: Any

  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = JUndefined
}

trait JScalarOf[+V] { _: JScalar =>
  def value: V
}

object JScalarOf {
  implicit def ordering[A](implicit vo: Ordering[A]): Ordering[JScalarOf[A]] = Ordering.by(_.value)
}

case object JNull extends JScalar with JScalarOf[Null] {
  def value: Null = null
}

sealed trait JBoolean extends JScalar with JScalarOf[Boolean]

object JBoolean {
  def apply(value: Boolean): JBoolean = if(value) JTrue else JFalse
  def unapply(b: JBoolean): Option[Boolean] = if(b == null) None else Some(b.value)
}

case object JTrue extends JBoolean {
  def value: Boolean = true
}

case object JFalse extends JBoolean {
  def value: Boolean = false
}

class JNumber(val value: Double) extends JScalar with JScalarOf[Double] {
  def unary_- : JNumber = new JNumber(-value)
  def + (r: JNumber.Repr): JNumber = new JNumber(this.value + r.value)
  def - (r: JNumber.Repr): JNumber = new JNumber(this.value - r.value)
  def * (r: JNumber.Repr): JNumber = new JNumber(this.value * r.value)
  def / (r: JNumber.Repr): JNumber = new JNumber(this.value * r.value)
  def % (r: JNumber.Repr): JNumber = new JNumber(this.value % r.value)

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JNumber => this.value == that.value
    case _ => false
  }

  override def toString: String = s"JNumber($value)"
}

object JNumber {
  def apply(repr: Repr): JNumber = new JNumber(repr.value)
  def unapply(n: JNumber): Option[Double] = if(n == null) None else Some(n.value)

  class Repr(val value: Double) extends AnyVal
  object Repr {
    implicit def ofJNumber(v: JNumber): Repr = new Repr(v.value)
    implicit def ofDouble(v: Double): Repr = new Repr(v)
    implicit def ofFloat(v: Float): Repr = new Repr(v.toDouble)
    implicit def ofInt(v: Int): Repr = new Repr(v.toDouble)
    implicit def ofShort(v: Short): Repr = new Repr(v.toDouble)
  }
}

case class JString(value: String) extends JScalar with JScalarOf[String]

object JString {
  def encode(reader: Reader, writer: Writer): Unit = {
    writer.write("\"")
    IOUtil.copy(reader, new StreamingJStringWriter(writer))
    writer.write("\"")
  }

  def encode(str: String, writer: Writer): Unit = {
    writer.write("\"")
    new StreamingJStringWriter(writer).write(str)
    writer.write("\"")
  }

  def encode(reader: Reader): String = {
    val sw = new java.io.StringWriter
    encode(reader, sw)
    sw.toString
  }

  def encode(str: CharSequence): String =
    encode(new java.io.StringReader(str.toString))
}
