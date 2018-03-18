package jameson
package dec

class InstanceJObjectReader(val path: JPath, obj: JObject) extends BaseJObjectReader {
  override protected def consume(f: (String, JReader) => Unit): Unit = {
    guard()

    val iterator = obj.entries.iterator
    while(iterator.hasNext) {
      val (name, value) = iterator.next()
      value match {
        case s: JString => using(new InstanceJStringReader(path/name, s.value)) { sr => f(name, sr) }
        case o: JObject => using(new InstanceJObjectReader(path/name, o)) { or => f(name, or) }
        case a: JArray => using(new InstanceJArrayReader(path/name, a)) { ar => f(name, ar) }
        case n: JNumber => f(name, n)
        case JNull => f(name, JNull)
        case b: JBoolean => f(name, b)
      }
    }
  }
}
