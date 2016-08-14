package jameson
package impl

class InstanceJObjectReader(obj: JObject) extends BaseJObjectReader {
  override protected def foreach(f: (String, JReader) => Unit): Unit = {
    guard()

    val iterator = obj.seq.iterator
    while(iterator.hasNext) {
      val (name, value) = iterator.next()
      value match {
        case s: JString => using(new InstanceJStringReader(s.value)) { sr => f(name, sr) }
        case o: JObject => using(new InstanceJObjectReader(o)) { or => f(name, or) }
        case a: JArray => using(new InstanceJArrayReader(a)) { ar => f(name, ar) }
        case n: JNumber => f(name, n)
        case JNull => f(name, JNull)
        case b: JBoolean => f(name, b)
      }
    }
  }
}
