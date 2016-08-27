package jameson
package enc

trait JObjectWriter {
  /** The path of the object being written. */
  def path: JPath

  /** Writes a property with the specified name and value, encoded according to an
    * implicitly provided [[JEncoder]].
    *
    * @param name The name of the property.
    * @param a The value to be encoded.
    * @param encoder The implicitly provided encoder for the value.
    * @tparam A The type of the value to be encoded.
    */
  def write[A](name: String, a: A)(implicit encoder: JEncoder[A]): JObjectWriter

  /** Writes a property with an array value.
    *
    * @param name The name of the property.
    * @param sizeHint Best guess as to the number of elements in the array. A negative
    *   value indicates that the number of elements is unknown.
    */
  def writeArray(name: String, sizeHint: Int)(loan: JArrayWriter => Unit): JObjectWriter

  /** Writes a property with an object value.
    *
    * @param name The name of the property.
    * @param sizeHint Best guess as to the number of properties in the object. A negative
    *   value indicates that the number of properties is unknown.
    */
  def writeObject(name: String, sizeHint: Int)(loan: JObjectWriter => Unit): JObjectWriter

  /** Writes a property with a string value.
    *
    * @param name The name of the property.
    */
  def writeString(name: String)(loan: JStringWriter => Unit): JObjectWriter

  /** Writes a property with an array value.
    *
    * @param name The name of the property.
    */
  def writeArray(name: String)(loan: JArrayWriter => Unit): JObjectWriter =
    writeArray(name, -1)(loan)

  /** Writes a property with an array value where the elements of the array are based on
    * the elements of the provided collection.
    *
    * @param name The name of the property.
    * @param coll The collection whose elements will form the basis for the array.
    * @tparam A The element type of the provided collection.
    */
  def writeArray[A](name: String)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): JObjectWriter =
    writeArray(name, coll.size) { arrayWriter => coll foreach { a => each(a, arrayWriter) } }

  /** Writes a property with an object value.
    *
    * @param name The name of the property.
    */
  def writeObject(name: String)(loan: JObjectWriter => Unit): JObjectWriter =
    writeObject(name, -1)(loan)

  /** Writes a property with an object value where the properties of the object are based on
    * the elements of the provided collection.
    *
    * @param name The name of the property.
    * @param coll The collection whose elements will form the basis for the object.
    * @tparam A The element type of the provided collection.
    */
  def writeObject[A](name: String)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): JObjectWriter =
    writeObject(name, coll.size) { objectWriter => coll foreach { a => each(a, objectWriter) } }
}
