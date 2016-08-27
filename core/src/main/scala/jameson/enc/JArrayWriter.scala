package jameson
package enc

trait JArrayWriter {
  def path: JPath

  def write[A](a: A)(implicit encoder: JEncoder[A]): JArrayWriter
  def writeArray(sizeHint: Int)(loan: JArrayWriter => Unit): JArrayWriter
  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): JArrayWriter
  def writeString(loan: JStringWriter => Unit): JArrayWriter

  def writeArray(loan: JArrayWriter => Unit): JArrayWriter =
    writeArray(-1)(loan)

  def writeArray[A](coll: Traversable[A])(each: (A, JArrayWriter) => Unit): JArrayWriter =
    writeArray(coll.size) { arrayWriter => coll foreach { a => each(a, arrayWriter) } }

  def writeObject(loan: JObjectWriter => Unit): JArrayWriter =
    writeObject(-1)(loan)

  def writeObject[A](coll: Traversable[A])(each: (A, JObjectWriter) => Unit): JArrayWriter =
    writeObject(coll.size) { objectWriter => coll foreach { a => each(a, objectWriter) } }
}
