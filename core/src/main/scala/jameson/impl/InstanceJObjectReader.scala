package jameson
package impl
import scala.collection.{immutable => sci, mutable => scm}

class InstanceJObjectReader(obj: JObject) extends BaseJObjectReader {
  override protected def foreach(f: (String, JReader) => Unit): Unit = {
    guard()

    obj.seq foreach { case (name, value) => f(name, value.reader) }
  }

  override protected def newBuilder[A]: scm.Builder[A, Seq[A]] = {
    val builder = Vector.newBuilder[A]
    if(obj.seq.isInstanceOf[sci.IndexedSeq[_]])
      builder.sizeHint(obj.seq.length)

    builder
  }
}
