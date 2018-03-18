package jameson

import java.io.Reader
import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable => sci, mutable => scm}
import scala.language.implicitConversions

sealed trait JValue {
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

  override def toString: String = s"JNumber($value)"
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
}

trait JStringReader extends Reader with JReader {
  def path: JPath
  def readAll(): String
  def copy(): JString
  override def toString: String = s"JStringReader($path)"
}

class JArray(private val elements: sci.IndexedSeq[JValue])
  extends JValue
  with scala.collection.immutable.IndexedSeq[JValue]
  with scala.collection.IndexedSeqLike[JValue, JArray]
{
  def apply(index: Int): JValue = elements(index)

  def length: Int = elements.length

  def valueType: JValue.Type = JArray

  override def newBuilder: JArray.Builder = JArray.newBuilder

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
  def apply(es: sci.IndexedSeq[JValue]): JArray = new JArray(es)
  def apply(es: Iterable[JValue]): JArray = new JArray(es.toIndexedSeq)

  def apply(e0: JValue.Repr[JValue], es: JValue.Repr[JValue]*): JArray = {
    val builder = JArray.newBuilder
    builder += e0.value
    var i = 0
    for(e <- es) {
      builder += e.value
    }
    builder.result()
  }

  def unapplySeq(a: JArray): Option[IndexedSeq[JValue]] = if(a == null) None else Some(a.elements)

  def newBuilder: Builder = new Builder()

  class Builder protected (private var underlying: scm.Builder[JValue, sci.IndexedSeq[JValue]])
    extends scala.collection.mutable.Builder[JValue, JArray]
  {
    def this() = this(Vector.newBuilder)

    def result(): JArray = new JArray(underlying.result())

    def add(elem: JValue): Builder.this.type = {
      underlying += elem
      this
    }

    def += (elem: JValue): Builder.this.type = add(elem)

    def clear(): Unit = {
      underlying.clear()
    }
  }

  implicit def canBuildFrom: CanBuildFrom[Seq[JValue], JValue, JArray] = _canBuildFrom
  private object _canBuildFrom extends CanBuildFrom[Seq[JValue], JValue, JArray] {
    override def apply(from: Seq[JValue]): scala.collection.mutable.Builder[JValue, JArray] = newBuilder
    override def apply(): scala.collection.mutable.Builder[JValue, JArray] = newBuilder
  }
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

class JObject protected (val entries: sci.Seq[(String, JValue)])
  extends JValue
  with scala.collection.immutable.Map[String, JValue]
  with scala.collection.immutable.MapLike[String, JValue, JObject]
{
  def get(name: String): Option[JValue] = {
    val iterator = entries.iterator
    while(iterator.hasNext) {
      val entry = iterator.next()
      if(entry._1 == name) return Some(entry._2)
    }
    None
  }

  override def iterator: Iterator[(String, JValue)] = entries.iterator
  override def -(key: String): JObject = new JObject(entries.filter(_._1 != key))
  override def +[V1 >: JValue](kv: (String, V1)): Map[String, V1] = {
    val builder = Map.newBuilder[String, V1]
    builder ++= entries
    builder += kv
    builder.result()
  }

  override def empty: JObject = JObject.empty
  override def toSeq: sci.Seq[(String, JValue)] = entries

  def ++ (o: JObject): JObject = new JObject(entries ++ o.toSeq)
  def + (m0: JObject.Mapping, ms: JObject.Mapping*): JObject = {
    val builder = JObject.newBuilder(this).add(m0.tup)
    for(m <- ms)
      builder.add(m.tup)
    builder.result()
  }

  def valueType: JValue.Type = JObject

  override def hashCode(): Int = entries.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: JObject => this.entries == that.entries
    case _ => false
  }

  override def toString: String =
    entries.iterator.map({ case (n, v) => enc.EncodingJValueWriter.encodeString(n) + ": " + v.toString }).mkString("JObject(", ", ", ")")

}

object JObject extends JValue.Type {
  val empty: JObject = new JObject(Vector.empty)

  def apply(): JObject = empty
  def apply(entries: Map[String, JValue]): JObject = new JObject(entries.to[sci.Seq])
  def apply(entries: Iterable[(String, JValue)]): JObject = newBuilder.add(entries).result()

  def apply(m0: Mapping, ms: Mapping*): JObject = {
    val builder = newBuilder
    builder.add(m0.tup)
    for(m <- ms)
      builder.add(m.tup)
    builder.result()
  }

  def unapplySeq(o: JObject): Option[Seq[(String, JValue)]] = if (o == null) None else Some(o.entries)

  class Mapping(val tup: (String, JValue)) extends AnyVal
  object Mapping {
    implicit def fromTuple(tup: (String, JValue)): Mapping = new Mapping(tup)
  }

  def newBuilder: Builder = new Builder()
  def newBuilder(entries: JObject): Builder = new Builder(entries)

  class Builder protected (private var entries: sci.Seq[(String, JValue)])
    extends scala.collection.mutable.Builder[(String, JValue), JObject]
  {
    def this() = this(empty.toSeq)
    def this(o: JObject) = this(o.toSeq)

    private var keySet = if(entries.isEmpty) Set.empty[String] else {
      val builder = Set.newBuilder[String]
      for(e <- entries)
        builder += e._1
      builder.result()
    }

    override def result(): JObject = new JObject(entries)

    def add(elem: (String, JValue)): Builder.this.type = {
      val key = elem._1
      if(keySet.contains(key)) {
        entries = entries.filter(_._1 != key)
      } else {
        keySet = keySet + key
      }
      entries = entries :+ elem
      this
    }

    def add(elems: TraversableOnce[(String, JValue)]): Builder.this.type = this ++= elems

    override def +=(elem: (String, JValue)): Builder.this.type = add(elem)

    override def clear(): Unit = {
      keySet = Set.empty
      entries = Vector.empty
    }
  }

  implicit def canBuildFrom: CanBuildFrom[Map[String, JValue], (String, JValue), JObject] = _canBuildFrom
  private object _canBuildFrom extends CanBuildFrom[Map[String, JValue], (String, JValue), JObject] {
    override def apply(from: Map[String, JValue]): scm.Builder[(String, JValue), JObject] = newBuilder
    override def apply(): scm.Builder[(String, JValue), JObject] = newBuilder
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
