package jameson
package impl
import scala.collection.mutable

class InstanceJArrayReader(arr: JArray) extends BaseJArrayReader {
  override protected def foreach(f: (Int, JReader) => Unit): Unit = {
    guard()

    var i = 0
    val iterator = arr.elements.iterator
    while(iterator.hasNext) {
      val value = iterator.next()
      value match {
        case s: JString => using(new InstanceJStringReader(s.value)) { sr => f(i, sr) }
        case o: JObject => using(new InstanceJObjectReader(o)) { or => f(i, or) }
        case a: JArray => using(new InstanceJArrayReader(a)) { ar => f(i, ar) }
        case n: JNumber => f(i, n)
        case JNull => f(i, JNull)
        case b: JBoolean => f(i, b)
      }
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
