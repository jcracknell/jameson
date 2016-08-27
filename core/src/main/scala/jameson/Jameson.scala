package jameson

import jameson.dec._
import java.io.Reader

object Jameson {
  def decode[A](str: String)(loan: JReader => A): A =
    decode(new java.io.StringReader(str))(loan)

  def decode[A](reader: Reader)(loan: JReader => A): A = {
    val ctx = new JParsingContext(reader, JPath)
    JParser.parse(ctx)(loan)
  }

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
    encodeArray(writer, -1)(loan)

  def encodeArray(writer: Writer, sizeHint: Int)(loan: JArrayWriter => Unit): Unit =
    encodeArray(writer, sizeHint, new EncodingOptions())(loan)

  def encodeArray(writer: Writer, options: EncodingOptions)(loan: JArrayWriter => Unit): Unit =
    encodeArray(writer, -1, options)(loan)

  def encodeArray(writer: Writer, sizeHint: Int, options: EncodingOptions)(loan: JArrayWriter => Unit): Unit = {
    val ctx = new EncodingContext(writer, JPath, options)
    using(new EncodingJArrayWriter(ctx, options.arrayStyleAt(ctx.path, sizeHint))) { arrayWriter =>
      loan(arrayWriter)
    }
  }

  def encodeArrayFrom[A](coll: Traversable[A])(each: (A, JArrayWriter) => Unit): String =
    encodeArrayFrom(new EncodingOptions())(coll)(each)

  def encodeArrayFrom[A](options: EncodingOptions)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): String = {
    val sw = new StringWriter
    encodeArrayFrom(sw, options)(coll)(each)
    sw.toString
  }

  def encodeArrayFrom[A](writer: Writer)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): Unit =
    encodeArrayFrom(writer, new EncodingOptions())(coll)(each)

  def encodeArrayFrom[A](writer: Writer, options: EncodingOptions)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): Unit =
    encodeArray(writer, coll.size, options) { arrayWriter => coll foreach { a => each(a, arrayWriter) } }

  def encodeObject(loan: JObjectWriter => Unit): String = encodeObject(new EncodingOptions())(loan)

  def encodeObject(sizeHint: Int)(loan: JObjectWriter => Unit): String =
    encodeObject(sizeHint, new EncodingOptions())(loan)

  def encodeObject(options: EncodingOptions)(loan: JObjectWriter => Unit): String =
    encodeObject(-1, options)(loan)

  def encodeObject(sizeHint: Int, options: EncodingOptions)(loan: JObjectWriter => Unit): String = {
    val sw = new StringWriter
    encodeObject(sw, options)(loan)
    sw.toString
  }

  def encodeObject(writer: Writer)(loan: JObjectWriter => Unit): Unit =
    encodeObject(writer, new EncodingOptions())(loan)

  def encodeObject(writer: Writer, options: EncodingOptions)(loan: JObjectWriter => Unit): Unit =
    encodeObject(writer, -1, options)(loan)

  def encodeObject(writer: Writer, sizeHint: Int)(loan: JObjectWriter => Unit): Unit =
    encodeObject(writer, sizeHint, new EncodingOptions())(loan)

  def encodeObject(writer: Writer, sizeHint: Int, options: EncodingOptions)(loan: JObjectWriter => Unit): Unit = {
    val ctx = new EncodingContext(writer, JPath, options)
    using(new EncodingJObjectWriter(ctx, options.objectStyleAt(ctx.path, sizeHint))) { objectWriter =>
      loan(objectWriter)
    }
  }

  def encodeObjectFrom[A](coll: Traversable[A])(each: (A, JObjectWriter) => Unit): String =
    encodeObjectFrom(new EncodingOptions())(coll)(each)

  def encodeObjectFrom[A](options: EncodingOptions)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): String = {
    val sw = new StringWriter
    encodeObjectFrom(sw, options)(coll)(each)
    sw.toString
  }

  def encodeObjectFrom[A](writer: Writer)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): Unit =
    encodeObjectFrom(writer, new EncodingOptions())(coll)(each)

  def encodeObjectFrom[A](writer: Writer, options: EncodingOptions)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): Unit =
    encodeObject(writer, coll.size, options) { objectWriter => coll foreach { a => each(a, objectWriter) } }

  case class EncodingOptions(
    arrayStyle: JArrayStyle = JArrayStyle.Compact,
    escapeNonASCII: Boolean = false,
    escapeQuotes: Boolean = false,
    escapeSlashes: Boolean = false,
    objectStyle: JObjectStyle = JObjectStyle.Compact
  ) extends jameson.enc.JEncodingOptions {
    def arrayStyleAt(path: JPath, sizeHint: Int): JArrayStyle = arrayStyle.at(path, sizeHint)
    def objectStyleAt(path: JPath, sizeHint: Int): JObjectStyle = objectStyle.at(path, sizeHint)
  }
}
