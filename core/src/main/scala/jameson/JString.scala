package jameson

import java.io.{Reader, Writer}

object JString {
  def encode(str: CharSequence, writer: Writer): Unit = {
    writer.write("\"")
    using(new JStringWriter(writer)) { writer =>
      writer.write(str.toString)
    }
    writer.write("\"")
  }

  def encode(str: CharSequence): String = {
    val sw = new java.io.StringWriter
    encode(str, sw)
    sw.toString
  }
}
