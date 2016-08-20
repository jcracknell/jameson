package jameson
package enc

trait JArrayWriter {
  def write[A](a: A)(implicit encoder: JEncoder[A]): JArrayWriter
  def writeArray(loan: JArrayWriter => Unit): JArrayWriter = writeArray(-1)(loan)
  def writeArray(sizeHint: Int)(loan: JArrayWriter => Unit): JArrayWriter
  def writeObject(loan: JObjectWriter => Unit): JArrayWriter = writeObject(-1)(loan)
  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): JArrayWriter
  def writeString(loan: JStringWriter => Unit): JArrayWriter
}
