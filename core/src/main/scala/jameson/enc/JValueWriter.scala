package jameson
package enc

/** Core encoding interface representing a consumable writer which can be used to
  * write out a single JSON value.
  */
trait JValueWriter {
  def writeArray(arr: JArray): Unit
  def writeArray(sizeHint: Int)(loan: JArrayWriter => Unit): Unit
  def writeBoolean(value: Boolean): Unit
  def writeBoolean(bool: JBoolean): Unit
  def writeNull(): Unit
  def writeNumber(value: Double): Unit
  def writeNumber(number: JNumber): Unit
  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): Unit
  def writeObject(obj: JObject): Unit
  def writeString(loan: JStringWriter => Unit): Unit
  def writeString(value: String): Unit
  def writeString(str: JString): Unit

  def writeArray(loan: JArrayWriter => Unit): Unit =
    writeArray(-1)(loan)

  def writeArray[A](coll: Traversable[A])(each: (A, JArrayWriter) => Unit): Unit =
    writeArray(coll.size) { arrayWriter => coll foreach { a => each(a, arrayWriter) } }

  def writeObject(loan: JObjectWriter => Unit): Unit =
    writeObject(-1)(loan)

  def writeObject[A](coll: Traversable[A])(each: (A, JObjectWriter) => Unit): Unit =
    writeObject(coll.size) { objectWriter => coll foreach { a => each(a, objectWriter) } }
}
