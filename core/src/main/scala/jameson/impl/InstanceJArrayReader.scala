package jameson
package impl
import scala.collection.mutable

class InstanceJArrayReader(arr: JArray) extends BaseJArrayReader {
  override protected def foreach(f: JReader => Unit): Unit = {
    guard()

    arr.elements foreach { value => f(value.reader) }
  }

  override protected def newBuilder[A]: mutable.Builder[A, IndexedSeq[A]] = {
    val builder = Vector.newBuilder[A]
    builder.sizeHint(arr.elements.length)
    builder
  }

  override def copy(): JArray = {
    guard()
    arr
  }
}
