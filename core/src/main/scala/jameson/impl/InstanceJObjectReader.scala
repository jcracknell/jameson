package jameson
package impl

class InstanceJObjectReader(obj: JObject) extends BaseJObjectReader {
  override protected def foreach(f: (String, JReader) => Unit): Unit = {
    guard()

    val iterator = obj.seq.iterator
    while(iterator.hasNext) {
      val (name, value) = iterator.next()
      f(name, value.reader)
    }
  }
}
