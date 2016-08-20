package jameson
package enc

import java.io.{StringWriter, Writer}

trait EncodingApi {
  def encode[A](a: A)(implicit encoder: JEncoder[A]): String =
    encode(a, new EncodingOptions())

  def encode[A](a: A, options: EncodingOptions)(implicit encoder: JEncoder[A]): String = {
    val sw = new StringWriter
    encode(a, sw, options)
    sw.toString
  }

  def encode[A](a: A, writer: Writer)(implicit encoder: JEncoder[A]): Unit =
    encode(a, writer, new EncodingOptions())

  def encode[A](a: A, writer: Writer, options: EncodingOptions)(implicit encoder: JEncoder[A]): Unit = {
    val ctx = new EncodingContext(writer, JPath, options)
    using(new EncodingJValueWriter(ctx)) { valueWriter =>
      encoder.encode(a, valueWriter)
    }
  }

  def encodeArray(loan: JArrayWriter => Unit): String = encodeArray(new EncodingOptions())(loan)

  def encodeArray(options: EncodingOptions)(loan: JArrayWriter => Unit): String = {
    val sw = new StringWriter
    encodeArray(sw, options)(loan)
    sw.toString
  }

  def encodeArray(writer: Writer)(loan: JArrayWriter => Unit): Unit =
    encodeArray(writer, new EncodingOptions())(loan)

  def encodeArray(writer: Writer, options: EncodingOptions)(loan: JArrayWriter => Unit): Unit = {
    val ctx = new EncodingContext(writer, JPath, options)
    using(new EncodingJArrayWriter(ctx)) { arrayWriter =>
      loan(arrayWriter)
    }
  }

  def encodeObject(loan: JObjectWriter => Unit): String = encodeObject(new EncodingOptions())(loan)

  def encodeObject(options: EncodingOptions)(loan: JObjectWriter => Unit): String = {
    val sw = new StringWriter
    encodeObject(sw, options)(loan)
    sw.toString
  }

  def encodeObject(writer: Writer)(loan: JObjectWriter => Unit): Unit =
    encodeObject(writer, new EncodingOptions())(loan)

  def encodeObject(writer: Writer, options: EncodingOptions)(loan: JObjectWriter => Unit): Unit = {
    val ctx = new EncodingContext(writer, JPath, options)
    using(new EncodingJObjectWriter(ctx)) { objectWriter =>
      loan(objectWriter)
    }
  }

  case class EncodingOptions(
    escapeNonASCII: Boolean = false,
    escapeQuotes: Boolean = false,
    escapeSlashes: Boolean = false
  ) extends jameson.enc.EncodingOptions
}
