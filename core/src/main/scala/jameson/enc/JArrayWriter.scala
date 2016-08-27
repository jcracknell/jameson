package jameson
package enc

trait JArrayWriter {
  /** The path of the array being written. */
  def path: JPath

  /** Writes an element with the specified value, encoded according to an implicitly
    * provided [[JEncoder]].
    *
    * @param a The value to be encoded.
    * @param encoder The implicitly provided encoder for the value.
    * @tparam A The type of the value to be encoded.
    */
  def write[A](a: A)(implicit encoder: JEncoder[A]): JArrayWriter

  /** Writes an element with an array value.
    *
    * @param sizeHint Best guess as to the number of elements in the array. A negative
    *   value indicates that the number of elements is unknown.
    */
  def writeArray(sizeHint: Int)(loan: JArrayWriter => Unit): JArrayWriter

  /** Writes an element with an object value.
    *
    * @param sizeHint Best guess as to the number of properties in the object. A negative
    *   value indicates that the number of properties is unknown.
    */
  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): JArrayWriter

  /** Writes an element with a string value. */
  def writeString(loan: JStringWriter => Unit): JArrayWriter

  /** Writes an element with an array value. */
  def writeArray(loan: JArrayWriter => Unit): JArrayWriter =
    writeArray(-1)(loan)

  /** Writes an element with an array value where the elements of the array
    * are based on those of the provided collection.
    *
    * @param coll The collection whose elements will form the basis for the array.
    * @tparam A The element type of the provided collection.
    */
  def writeArray[A](coll: Traversable[A])(each: (A, JArrayWriter) => Unit): JArrayWriter =
    writeArray(coll.size) { arrayWriter => coll foreach { a => each(a, arrayWriter) } }

  /** Writes an element with an object value. */
  def writeObject(loan: JObjectWriter => Unit): JArrayWriter =
    writeObject(-1)(loan)

  /** Writes an element with an object value where the properties of the object are based
    * on the elements of the provided collection.
    *
    * @param coll The collection whose elements will form the basis for the object.
    * @tparam A The element type of the provided collection.
    */
  def writeObject[A](coll: Traversable[A])(each: (A, JObjectWriter) => Unit): JArrayWriter =
    writeObject(coll.size) { objectWriter => coll foreach { a => each(a, objectWriter) } }
}
