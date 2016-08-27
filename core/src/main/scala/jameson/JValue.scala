package jameson

import java.io.{Reader, Writer}
import jameson.enc._
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
  def valueType: JValue.Type
}

object JValue {
  /** Algebraic data type representing the types of [[JValue]] available.
    * Implemented by [[JValue]] companion objects.
    */
  sealed trait Type

  class Repr[+J <: JValue](val value: J) extends AnyVal
  object Repr {
    implicit def fromJValue[J <: JValue](v: J): Repr[J] = new Repr(v)
    implicit def fromNull(v: Null): Repr[JNull.type] = new Repr(JNull)
    implicit def fromBoolean(v: Boolean): Repr[JBoolean] = new Repr(JBoolean(v))
    implicit def fromDouble(v: Double): Repr[JNumber] = new Repr(new JNumber(v))
    implicit def fromFloat(v: Float): Repr[JNumber] = new Repr(new JNumber(v.toDouble))
    implicit def fromInt(v: Int): Repr[JNumber] = new Repr(new JNumber(v.toDouble))
    implicit def fromShort(v: Short): Repr[JNumber] = new Repr(new JNumber(v.toDouble))
    implicit def fromString(v: String): Repr[JString] = new Repr(new JString(v))
  }
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

sealed trait JScalar extends JValue {
  /** Get the native representation of this scalar JSON value. */
  def value: Any

  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = JUndefined
}

case object JNull extends JScalar with JReader with JValue.Type {
  def value: Null = null
  def copy(): JNull.type = this
  def discard(): Unit = { }
  def valueType: JValue.Type = JNull
}

sealed trait JBoolean extends JScalar with JReader {
  def value: Boolean
  def copy(): JBoolean = this
  def discard(): Unit = { }
  def valueType: JValue.Type = JBoolean
}

object JBoolean extends JValue.Type {
  def apply(value: Boolean): JBoolean = if(value) JTrue else JFalse
  def unapply(b: JBoolean): Option[Boolean] = if(b == null) None else Some(b.value)

  implicit val ordering: Ordering[JBoolean] = Ordering.by(_.value)
}

case object JTrue extends JBoolean {
  def value: Boolean = true
}

case object JFalse extends JBoolean {
  def value: Boolean = false
}

class JNumber(val value: Double) extends JScalar with JReader {
  def copy(): JNumber = this
  def discard(): Unit = { }
  def valueType: JValue.Type = JNumber

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JNumber => this.value == that.value
    case _ => false
  }

  override def toString: String = "JNumber(" + value.toString + ")"
}

object JNumber extends JValue.Type {
  def apply(repr: JValue.Repr[JNumber]): JNumber = repr.value

  def unapply(n: JNumber): Option[Double] = if(n == null) None else Some(n.value)

  implicit val ordering: Ordering[JNumber] = Ordering.by(_.value)
}

case class JString(value: String) extends JScalar {
  def valueType: JValue.Type = JString
}

object JString extends JValue.Type {
  val empty: JString = new JString("")

  implicit val ordering: Ordering[JString] = Ordering.by(_.value)

  def encode(str: String, writer: Writer): Unit = EncodingJStringWriter.encode(str, writer)

  def encode(str: String): String = EncodingJStringWriter.encode(str)
}

trait JStringReader extends Reader with JReader {
  def path: JPath
  def copy(): JString
  override def toString: String = s"JStringReader($path)"
}

class JArray(val elements: scala.collection.immutable.IndexedSeq[JValue]) extends JValue {
  def apply(name: String): JLookup = JUndefined
  def apply(index: Int): JLookup = if(elements.isDefinedAt(index)) elements(index) else JUndefined

  def length: Int = elements.length

  def valueType: JValue.Type = JArray

  override def hashCode(): Int = elements.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JArray => this.elements == that.elements
    case _ => false
  }

  override def toString: String = elements.mkString("JArray(", ", ", ")")
}

object JArray extends JValue.Type {
  val empty: JArray = new JArray(Vector.empty)

  def apply(): JArray = empty
  def apply(es: Seq[JValue]): JArray = new JArray(es.toVector)
  def apply(e0: JValue.Repr[JValue], es: JValue.Repr[JValue]*): JArray = apply(e0.value +: es.view.map(_.value))

  def unapplySeq(a: JArray): Option[IndexedSeq[JValue]] = if(a == null) None else Some(a.elements)
}

trait JArrayReader extends JReader {
  def path: JPath
  def copy(): JArray

  /** Consumes the reader, returning the array elements matched by the provided collector. */
  def collect[A](collector: PartialFunction[JReader, A]): Seq[A]

  /** Consumes the reader, returning the array elements matched by the provided collector. */
  def collectIndexed[A](collector: PartialFunction[(Int, JReader), A]): Seq[A]

  def collectAll[A](collector: PartialFunction[JReader, A]): (Seq[A], Seq[JValue])
  def collectAllIndexed[A](collector: PartialFunction[(Int, JReader), A]): (Seq[A], Seq[(Int, JValue)])

  def map[A](projection: JReader => A): IndexedSeq[A]
  def mapIndexed[A](projection: (Int, JReader) => A): IndexedSeq[A]

  override def toString: String = s"JArrayReader($path)"
}

class JObject protected (
  val seq: sci.Seq[(String, JValue)],
  val map: sci.Map[String, JValue]
) extends JValue {
  def apply(name: String): JLookup = map.get(name) match {
    case Some(v) => v
    case None => JUndefined
  }

  def apply(index: Int): JLookup = JUndefined

  def valueType: JValue.Type = JObject

  override def hashCode(): Int = seq.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JObject => this.seq == that.seq
    case _ => false
  }

  override def toString: String =
    seq.iterator.map({ case (n, v) => JString.encode(n) + ": " + v.toString }).mkString("JObject(", ", ", ")")
}

object JObject extends JValue.Type {
  val empty: JObject = new JObject(Vector.empty, Map.empty)

  def apply(): JObject = empty
  def apply(m0: Mapping, ms: Mapping*): JObject = builder.add(m0.tup).add(ms.view.map(_.tup)).result
  def apply(map: Map[String, JValue]): JObject = new JObject(map.to[sci.Seq], map)
  def apply(seq: Seq[(String, JValue)]): JObject = builder.add(seq).result

  def unapplySeq(o: JObject): Option[Seq[(String, JValue)]] = if(o == null) None else Some(o.seq)

  def builder: Builder = builder(JObject.empty)
  def builder(o: JObject): Builder = new BuilderImpl(o.seq, o.map)

  class Mapping(val tup: (String, JValue)) extends AnyVal
  object Mapping {
    implicit def fromTuple(tup: (String, JValue)): Mapping = new Mapping(tup)
  }

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

trait JObjectReader extends JReader {
  def path: JPath

  def capture[A](name: String)(collector: PartialFunction[JReader, A]): JObjectReader.Capture[A]
  def captureValue(name: String): JObjectReader.Capture[JValue]
  def collect[A](collector: PartialFunction[(String, JReader), A]): Seq[A]
  def collectAll[A](collector: PartialFunction[(String, JReader), A]): (Seq[A], Seq[(String, JValue)])
  def copy(): JObject

  override def toString: String = s"JObjectReader($path)"
}

object JObjectReader {
  trait Capture[+A] {
    def path: JPath.Property
    def name: String = path.name

    def get: A

    def wasCaptured: Boolean
    def wasPresent: Boolean
    def wasMismatch: Boolean = wasPresent && !wasCaptured
    def wasMissing: Boolean = !wasPresent

    def isDefined: Boolean = wasCaptured

    def toOption: Option[A] = if(wasCaptured) Some(get) else None
  }
}
