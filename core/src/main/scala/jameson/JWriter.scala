package jameson

import java.io.Writer

trait JWriter {
  def writeArray(loan: Unit => Unit): Unit
  def writeBoolean(value: Boolean): Unit
  def writeNull(): Unit
  def writeNumber(value: JNumber.Repr): Unit
  def writeObject(loan: JObjectWriter => Unit): Unit
  def writeString(loan: JStringWriter => Unit): Unit
  def writeString(value: String): Unit = writeString(_.write(value))
}

trait JStringWriter extends Writer

trait JArrayWriter {
  def writeArray(loan: JArrayWriter => Unit): JArrayWriter
  def writeBoolean(value: Boolean): JArrayWriter
  def writeNull(): JArrayWriter
  def writeNumber(value: JNumber.Repr): JArrayWriter
  def writeObject(loan: JObjectWriter => Unit): JArrayWriter
  def writeString(loan: JStringWriter => Unit): JArrayWriter
  def writeString(value: String): Unit = writeString(_.write(value))
}

trait JObjectWriter {
  def writeArray(name: String)(loan: JArrayWriter => Unit): JObjectWriter
  def writeBoolean(name: String, value: Boolean): JObjectWriter
  def writeNull(name: String): JObjectWriter
  def writeNumber(name: String, value: JNumber.Repr): JObjectWriter
  def writeObject(name: String)(loan: JObjectWriter => Unit): JObjectWriter
  def writeString(name: String)(loan: JStringWriter => Unit): JObjectWriter
  def writeString(name: String, value: String): JObjectWriter = writeString(name)(_.write(value))
}
