package jameson.enc

import java.io.Writer


trait JArrayWriter {
  def write[A](a: A)(implicit encoder: JEncoder[A]): JArrayWriter
  def writeArray(loan: JArrayWriter => Unit): JArrayWriter
  def writeObject(loan: JObjectWriter => Unit): JArrayWriter
  def writeString(loan: JStringWriter => Unit): JArrayWriter
}

