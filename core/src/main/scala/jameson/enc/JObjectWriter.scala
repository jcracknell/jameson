package jameson
package enc

trait JObjectWriter {
  def write[A](name: String, a: A)(implicit encoder: JEncoder[A]): JObjectWriter
  def writeArray(name: String)(loan: JArrayWriter => Unit): JObjectWriter = writeArray(name, -1)(loan)
  def writeArray(name: String, sizeHint: Int)(loan: JArrayWriter => Unit): JObjectWriter
  def writeObject(name: String)(loan: JObjectWriter => Unit): JObjectWriter = writeObject(name, -1)(loan)
  def writeObject(name: String, sizeHint: Int)(loan: JObjectWriter => Unit): JObjectWriter
  def writeString(name: String)(loan: JStringWriter => Unit): JObjectWriter
}
