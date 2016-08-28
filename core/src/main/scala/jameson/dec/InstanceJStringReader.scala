package jameson
package dec

import jameson.util.IOUtil

class InstanceJStringReader(val path: JPath, str: String) extends JStringReader {
  private var closed = false
  private var rp = 0

  override def read(): Int = {
    assertOpen()

    if(rp >= str.length) -1 else {
      val c = str.charAt(rp)
      rp += 1
      c.toInt
    }
  }

  override def read(chars: Array[Char], offset: Int, length: Int): Int = {
    assertOpen()

    if(rp >= str.length) 0 else {
      val start = rp
      val end = rp + length
      while(rp < end) {
        chars(rp - start) = str.charAt(rp)
        rp += 1
      }
      rp - start
    }
  }

  def readAll(): String = {
    assertOpen()

    val result = if(0 == rp) str else str.substring(rp)
    rp = str.length
    result
  }

  def discard(): Unit = {
    assertOpen()

    rp = str.length
  }

  def copy(): JString = {
    assertOpen()

    val result = new JString(str.substring(rp, str.length))
    rp = str.length
    result
  }

  override def close(): Unit = {
    if(rp < str.length) throw new IllegalStateException("Attempt to close unconsumed JStringReader.")

    closed = true
  }

  @inline private def assertOpen(): Unit =
    if(closed) throw new IllegalStateException("Attempt to access closed JStringReader.")
}
