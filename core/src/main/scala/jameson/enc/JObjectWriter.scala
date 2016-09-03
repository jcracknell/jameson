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

  /** Writes a property with an object value.
    *
    * @param name The name of the property.
    */
  def writeObject(name: String)(loan: JObjectWriter => Unit): JObjectWriter =
    writeObject(name, -1)(loan)
}
