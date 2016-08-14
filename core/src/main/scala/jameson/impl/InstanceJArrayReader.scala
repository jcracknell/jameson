package jameson
package impl
import scala.collection.mutable

class InstanceJArrayReader(arr: JArray) extends BaseJArrayReader {
  override protected def foreach(f: (Int, JReader) => Unit): Unit = {
    guard()

    var i = 0
    arr.elements foreach { value =>
      f(i, value.reader)
      i += 1
    }
  }

  override protected def indexedSeqBuilder[A]: mutable.Builder[A, IndexedSeq[A]] = {
    val builder = Vector.newBuilder[A]
    builder.sizeHint(arr.elements.length)
    builder
  }

  override def copy(): JArray = {
    guard()
    arr
  }
}
