package jameson

import java.io.Reader
import scala.annotation.tailrec

/** [[java.io.Reader]] implementation which decodes a JSON string from an underlying
  * [[java.io.Reader]]. Reading ends when a closing U+0022 QUOTATION MARK character
  * is encountered. Note that the leading quotation mark should have been consumed
  * from the reader.
  */
class JStringReader(reader: Reader) extends Reader {
  private var ended = false
  private var closed = false

  override def read(chars: Array[Char], offset: Int, length: Int): Int = {
    if(closed)
      throw new UnsupportedOperationException(s"Attempt to access closed ${getClass.getName}.")

    val end = offset + length
    @tailrec def loop(i: Int): Int = if(i >= end) i - offset else {
      val c = read()
      if(c < 0) i - offset else {
        chars(i) = c.toChar
        loop(i + 1)
      }
    }
    loop(offset)
  }

  override def read(): Int = {
    if(closed)
      throw new UnsupportedOperationException(s"Attempt to access closed ${getClass.getName}.")

    if(ended) -1 else reader.read() match {
      case 0x5C => readEscape()
      case 0x22 =>
        ended = true
        -1
      case c =>
        if(Character.isISOControl(c))
          throw new JParsingException("Invalid character in JSON string.")
        if(c < 0)
          throw new JParsingException("Unexpected end of input in JSON string.")
        c
    }
  }

  private def readEscape(): Int = reader.read() match {
    case 0x22 => 0x22 // '"'
    case 0x5C => 0x5C // '\\'
    case 0x2F => 0x2F // '/'
    case 0x62 => 0x08 // '\b'
    case 0x66 => 0x0C // '\f'
    case 0x6E => 0x0A // '\n'
    case 0x72 => 0x0D // '\r'
    case 0x74 => 0x09 // '\t'
    case 0x75 => readUnicodeEscape()
    case _ => throw new JParsingException("Invalid escape sequence in JSON string.")
  }

  private def readUnicodeEscape(): Int = {
    def dec(c: Int): Int = c match {
      case 0x30        => 0x0
      case 0x31        => 0x1
      case 0x32        => 0x2
      case 0x33        => 0x3
      case 0x34        => 0x4
      case 0x35        => 0x5
      case 0x36        => 0x6
      case 0x37        => 0x7
      case 0x38        => 0x8
      case 0x39        => 0x9
      case 0x41 | 0x61 => 0xA
      case 0x42 | 0x62 => 0xB
      case 0x43 | 0x63 => 0xC
      case 0x44 | 0x64 => 0xD
      case 0x45 | 0x65 => 0xE
      case 0x46 | 0x66 => 0xF
      case c if c < 0 => throw new JParsingException("Unexpected end of input in JSON string.")
      case _ => throw new JParsingException("Invalid unicode escape.")
    }

    dec(reader.read()) << 12 | dec(reader.read()) << 8 | dec(reader.read()) << 4 | dec(reader.read())
  }

  /** Consumes the reader, discarding any further content. */
  def discard(): Unit = while(read() >= 0) {}

  /** Closes the reader, preventing any further access to public methods.
    * Throws an exception in the event that the reader has not been fully consumed.
    */
  override def close(): Unit = {
    if(!ended)
      throw new UnsupportedOperationException(s"Attempt to close unconsumed ${getClass.getName}.")

    closed = true
  }
}
