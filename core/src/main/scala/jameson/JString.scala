package jameson

import java.io.{Reader, Writer}

import jameson.util.IOUtil

object JString {
  def encode(reader: Reader, writer: Writer): Unit = {
    writer.write("\"")
    using(new JStringWriter(writer)) { jsw =>
      IOUtil.copy(reader, jsw)
    }
    writer.write("\"")
  }

  def encode(str: CharSequence, writer: Writer): Unit = {
    writer.write("\"")
    using(new JStringWriter(writer)) { jsw =>
      jsw.write(str.toString)
    }
    writer.write("\"")
  }

  def encode(reader: Reader): String = {
    val sw = new java.io.StringWriter
    encode(reader, sw)
    sw.toString
  }

  def encode(str: CharSequence): String =
    encode(new java.io.StringReader(str.toString))
}
