package jameson.util

import java.io.{Reader, Writer}

import scala.annotation.tailrec

object IOUtil {
  /** Copies the contents of the provided [[java.io.Reader]] to the provided [[java.io.Writer]]. */
  def copy(reader: Reader, writer: Writer): Unit = {
    val buffer = Array.ofDim[Char](4096)
    @tailrec def loop(): Unit = {
      val n = reader.read(buffer, 0, buffer.length)
      if(n > 0) {
        writer.write(buffer, 0, n)
        loop()
      }
    }
    loop()
  }
}
