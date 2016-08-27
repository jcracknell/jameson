package jameson
package dec

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
    if(rp < str.length) throw new UnsupportedOperationException("Attempt to close unconsumed JStringReader.")

    closed = true
  }

  @inline private def assertOpen(): Unit =
    if(closed) throw new UnsupportedOperationException("Attempt to access closed JStringReader.")
}
