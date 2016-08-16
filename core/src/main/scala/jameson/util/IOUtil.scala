package jameson.util

import java.io.{Reader, Writer}

import scala.annotation.tailrec

object IOUtil {
  val DEFAULT_BUFFER_SIZE = 4096

  /** Copies the contents of the provided [[java.io.Reader]] to the provided [[java.io.Writer]]. */
  def copy(reader: Reader, writer: Writer): Unit = copy(reader, writer, DEFAULT_BUFFER_SIZE)

  /** Copies the contents of the provided [[java.io.Reader]] to the provided [[java.io.Writer]]. */
  def copy(reader: Reader, writer: Writer, bufferSize: Int): Unit =
    copy(reader, writer, Array.ofDim[Char](bufferSize))

  def copy(reader: Reader, writer: Writer, buffer: Array[Char]): Unit = {
    @tailrec def loop(): Unit = {
      val n = reader.read(buffer, 0, buffer.length)
      if(n > 0) {
        writer.write(buffer, 0, n)
        loop()
      }
    }
    loop()
  }

  def readAll(reader: Reader): String = readAll(reader, DEFAULT_BUFFER_SIZE)

  def readAll(reader: Reader, bufferSize: Int): String =
    readAll(reader, Array.ofDim[Char](bufferSize))

  def readAll(reader: Reader, buffer: Array[Char]): String = {
    val sw = new java.io.StringWriter
    copy(reader, sw)
    sw.toString
  }
}
