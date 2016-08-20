package jameson

import jameson.enc._

trait JEncoder[A] {
  def encode(a: A, writer: JValueWriter): Unit
}

object JEncoder extends DefaultEncoders {
  def apply[A](f: (A, JValueWriter) => Unit): JEncoder[A] = new JEncoder[A] {
    def encode(a: A, ctx: JValueWriter): Unit = f(a, ctx)
  }

  def toArray[A](f: (A, JArrayWriter) => Unit): JEncoder[A] = new JEncoder[A] {
    def encode(a: A, writer: JValueWriter): Unit =
      writer writeArray { arrayWriter => f(a, arrayWriter) }
  }

  def toObject[A](f: (A, JObjectWriter) => Unit): JEncoder[A] = new JEncoder[A] {
    def encode(a: A, writer: JValueWriter): Unit =
      writer writeObject { objectWriter => f(a, objectWriter) }
  }
}
