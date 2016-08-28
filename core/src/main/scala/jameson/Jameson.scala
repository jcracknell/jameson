package jameson

import jameson.dec._
import jameson.enc._
import java.io.{Reader, StringWriter, Writer}

object Jameson {
  def read[A](str: String)(loan: JReader => A): A =
    read(str, new ParsingOptions())(loan)

  def read[A](str: String, options: ParsingOptions)(loan: JReader => A): A =
    read(new java.io.StringReader(str), options)(loan)

  def read[A](reader: Reader)(loan: JReader => A): A =
    read(reader, new ParsingOptions())(loan)

  def read[A](reader: Reader, options: ParsingOptions)(loan: JReader => A): A = {
    val ctx = new JParsingContext(reader, JPath, options)
    JParser.parse(ctx)(loan)
  }

  def write[A](a: A)(implicit encoder: JEncoder[A]): String =
    write(a, new EncodingOptions())

  def write[A](a: A, options: EncodingOptions)(implicit encoder: JEncoder[A]): String = {
    val sw = new StringWriter
    write(a, sw, options)
    sw.toString
  }

  def write[A](a: A, writer: Writer)(implicit encoder: JEncoder[A]): Unit =
    write(a, writer, new EncodingOptions())

  def write[A](a: A, writer: Writer, options: EncodingOptions)(implicit encoder: JEncoder[A]): Unit = {
    val ctx = new JEncodingContext(writer, JPath, options)
    using(new EncodingJValueWriter(ctx)) { valueWriter =>
      encoder.encode(a, valueWriter)
    }
  }

  def writeArray(loan: JArrayWriter => Unit): String = writeArray(new EncodingOptions())(loan)

  def writeArray(options: EncodingOptions)(loan: JArrayWriter => Unit): String = {
    val sw = new StringWriter
    writeArray(sw, options)(loan)
    sw.toString
  }

  def writeArray(writer: Writer)(loan: JArrayWriter => Unit): Unit =
    writeArray(writer, -1)(loan)

  def writeArray(writer: Writer, sizeHint: Int)(loan: JArrayWriter => Unit): Unit =
    writeArray(writer, sizeHint, new EncodingOptions())(loan)

  def writeArray(writer: Writer, options: EncodingOptions)(loan: JArrayWriter => Unit): Unit =
    writeArray(writer, -1, options)(loan)

  def writeArray(writer: Writer, sizeHint: Int, options: EncodingOptions)(loan: JArrayWriter => Unit): Unit = {
    val ctx = new JEncodingContext(writer, JPath, options)
    using(new EncodingJArrayWriter(ctx, options.arrayStyleAt(ctx.path, sizeHint))) { arrayWriter =>
      loan(arrayWriter)
    }
  }

  def writeArrayFrom[A](coll: Traversable[A])(each: (A, JArrayWriter) => Unit): String =
    writeArrayFrom(new EncodingOptions())(coll)(each)

  def writeArrayFrom[A](options: EncodingOptions)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): String = {
    val sw = new StringWriter
    writeArrayFrom(sw, options)(coll)(each)
    sw.toString
  }

  def writeArrayFrom[A](writer: Writer)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): Unit =
    writeArrayFrom(writer, new EncodingOptions())(coll)(each)

  def writeArrayFrom[A](writer: Writer, options: EncodingOptions)(coll: Traversable[A])(each: (A, JArrayWriter) => Unit): Unit =
    writeArray(writer, coll.size, options) { arrayWriter => coll foreach { a => each(a, arrayWriter) } }

  def writeObject(loan: JObjectWriter => Unit): String = writeObject(new EncodingOptions())(loan)

  def writeObject(sizeHint: Int)(loan: JObjectWriter => Unit): String =
    writeObject(sizeHint, new EncodingOptions())(loan)

  def writeObject(options: EncodingOptions)(loan: JObjectWriter => Unit): String =
    writeObject(-1, options)(loan)

  def writeObject(sizeHint: Int, options: EncodingOptions)(loan: JObjectWriter => Unit): String = {
    val sw = new StringWriter
    writeObject(sw, options)(loan)
    sw.toString
  }

  def writeObject(writer: Writer)(loan: JObjectWriter => Unit): Unit =
    writeObject(writer, new EncodingOptions())(loan)

  def writeObject(writer: Writer, options: EncodingOptions)(loan: JObjectWriter => Unit): Unit =
    writeObject(writer, -1, options)(loan)

  def writeObject(writer: Writer, sizeHint: Int)(loan: JObjectWriter => Unit): Unit =
    writeObject(writer, sizeHint, new EncodingOptions())(loan)

  def writeObject(writer: Writer, sizeHint: Int, options: EncodingOptions)(loan: JObjectWriter => Unit): Unit = {
    val ctx = new JEncodingContext(writer, JPath, options)
    using(new EncodingJObjectWriter(ctx, options.objectStyleAt(ctx.path, sizeHint))) { objectWriter =>
      loan(objectWriter)
    }
  }

  def writeObjectFrom[A](coll: Traversable[A])(each: (A, JObjectWriter) => Unit): String =
    writeObjectFrom(new EncodingOptions())(coll)(each)

  def writeObjectFrom[A](options: EncodingOptions)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): String = {
    val sw = new StringWriter
    writeObjectFrom(sw, options)(coll)(each)
    sw.toString
  }

  def writeObjectFrom[A](writer: Writer)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): Unit =
    writeObjectFrom(writer, new EncodingOptions())(coll)(each)

  def writeObjectFrom[A](writer: Writer, options: EncodingOptions)(coll: Traversable[A])(each: (A, JObjectWriter) => Unit): Unit =
    writeObject(writer, coll.size, options) { objectWriter => coll foreach { a => each(a, objectWriter) } }

  case class ParsingOptions(
    allowComments: Boolean = false,
    allowSingleQuotes: Boolean = false
  ) extends JParsingOptions

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
