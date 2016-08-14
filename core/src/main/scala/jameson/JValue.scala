package jameson

import jameson.impl._
import jameson.util.IOUtil
import java.io.{Reader, Writer}
import scala.collection.{immutable => sci}

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
  // TODO: loan control
  def reader: JReader
}

sealed trait JReader {
  /** Consumes the reader, marshaling its contents to the equivalent [[JValue]]
    * representation.
    */
  def copy(): JValue

  /** Consumes the reader, discarding any further content. */
  def discard(): Unit

  /** Convenience method. Invokes `discard` and returns the provided value. */
  def discard[A](a: A): A = {
    discard()
    a
  }
}

trait JReaderOf[+J <: JValue] { _: JReader =>
  override def copy(): J
}

sealed trait JScalar extends JValue {
  /** Get the native representation of this scalar JSON value. */
  def value: Any

  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = JUndefined
}

trait JScalarOf[+V] { _: JScalar =>
  override def value: V
}

object JScalarOf {
  implicit def ordering[A](implicit vo: Ordering[A]): Ordering[JScalarOf[A]] = Ordering.by(_.value)
}

trait JAtom[+V, +J <: JAtom[V, J]] extends JScalar with JScalarOf[V]
                                      with JReader with JReaderOf[J]
{
  def reader: J = this.asInstanceOf[J]
  def discard(): Unit = { }
  def copy(): J = this.asInstanceOf[J]
}

case object JNull extends JNull
sealed trait JNull extends JAtom[Null, JNull] {
  def value: Null = null
}

sealed trait JBoolean extends JAtom[Boolean, JBoolean]

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

class JNumber(val value: Double) extends JAtom[Double, JNumber] {
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

case class JString(value: String) extends JScalar with JScalarOf[String] {
  def reader: JStringReader = new InstanceJStringReader(value)
}

object JString {
  val empty: JString = new JString("")

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

trait JStringReader extends Reader with JReader {
  def copy(): JString
}

class JArray(val elements: scala.collection.immutable.IndexedSeq[JValue]) extends JValue {
  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = if(elements.isDefinedAt(index)) elements(index) else JUndefined

  def length: Int = elements.length

  def reader: JArrayReader = new InstanceJArrayReader(this)

  override def hashCode(): Int = elements.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JArray => this.elements == that.elements
    case _ => false
  }

  override def toString: String = elements.mkString("JArray(", ", ", ")")
}

object JArray {
  val empty: JArray = new JArray(Vector.empty)

  def apply(): JArray = empty
  def apply(es: Seq[JValue]): JArray = new JArray(es.toVector)
  def apply(e0: JValue, es: JValue*): JArray = apply(e0 +: es)

  def unapplySeq(a: JArray): Option[IndexedSeq[JValue]] = if(a == null) None else Some(a.elements)
}

trait JArrayReader extends JReader with JReaderOf[JArray] {
  /** Consumes the reader, returning the array elements matched by the provided collector. */
  def collect[A](collector: PartialFunction[JReader, A]): Seq[A]

  /** Consumes the reader, returning the array elements matched by the provided collector. */
  def collectIndexed[A](collector: PartialFunction[(Int, JReader), A]): Seq[A]

  def collectAll[A](collector: PartialFunction[JReader, A]): (Seq[A], Seq[JValue])
  def collectAllIndexed[A](collector: PartialFunction[(Int, JReader), A]): (Seq[A], Seq[(Int, JValue)])

  def map[A](projection: JReader => A): IndexedSeq[A]
  def mapIndexed[A](projection: (Int, JReader) => A): IndexedSeq[A]
}

class JObject protected (
  val seq: sci.Seq[(String, JValue)],
  val map: sci.Map[String, JValue]
) extends JValue {
  def reader[A](loan: JReader => A): A = using(new InstanceJObjectReader(this))(loan)

  def apply(name: String): JLookup = map.get(name) match {
    case Some(v) => v
    case None => JUndefined
  }

  def apply(index: Int): JLookup = JUndefined

  def reader: JObjectReader = new InstanceJObjectReader(this)

  override def hashCode(): Int = seq.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JObject => this.seq == that.seq
    case _ => false
  }

  override def toString: String =
    seq.iterator.map({ case (n, v) => JString.encode(n) + ": " + v.toString }).mkString("JObject(", ", ", ")")
}

object JObject {
  val empty: JObject = new JObject(Vector.empty, Map.empty)

  def apply(): JObject = empty
  def apply(m0: (String, JValue), ms: (String, JValue)*): JObject = builder.add(m0).add(ms).result
  def apply(map: Map[String, JValue]): JObject = new JObject(map.to[sci.Seq], map)
  def apply(seq: Seq[(String, JValue)]): JObject = builder.add(seq).result

  def unapplySeq(o: JObject): Option[Seq[(String, JValue)]] = if(o == null) None else Some(o.seq)

  def builder: Builder = builder(JObject.empty)
  def builder(o: JObject): Builder = new BuilderImpl(o.seq, o.map)

  trait Builder {
    def result: JObject
    def add(m: (String, JValue)): Builder
    def add(ms: Iterable[(String, JValue)]): Builder = ms.foldLeft(this)(_ add _)
    def add(m0: (String, JValue), ms: (String, JValue)*): Builder = add(m0).add(ms)
  }

  protected class BuilderImpl(
    protected var seq: sci.Seq[(String, JValue)],
    protected var map: sci.Map[String, JValue]
  ) extends Builder {
    def result: JObject = new JObject(seq, map)

    def add(m: (String, JValue)): Builder = {
      val key = m._1
      if(map.contains(key)) {
        seq = seq.filter(_._1 != key) :+ m
      } else {
        seq :+= m
      }
      map += m
      this
    }
  }
}

trait JObjectReader extends JReader with JReaderOf[JObject] {
  def collect[A](collector: PartialFunction[(String, JReader), A]): Seq[A]
}
