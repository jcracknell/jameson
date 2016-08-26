package jameson
package dec

import java.io.Reader

import scala.annotation.tailrec

/** The parsing context is a buffering [[java.io.Reader]] wrapper which tracks
  * its location for debugging purposes and provides parsing-related convenience
  * methods.
  */
final class ParsingContext(reader: Reader, private var _path: JPath) extends Reader {
  private val _buffer = Array.ofDim[Char](128)
  private var _bufferPos = 0
  private var _bufferFill = 0

  private var _atEOF = false
  private var _lineNumber = 1
  private var _columnIndex = 0
  private var _charIndex = 0

  def path: JPath = _path
  def path(index: Int): Unit = { _path /= index }
  def path(name: String): Unit = { _path /= name }
  def pathUp(): Unit = { _path = path.parent }

  def lineNumber: Int = _lineNumber
  def columnIndex: Int = _columnIndex
  def charIndex: Int = _charIndex
  def location: ParsingLocation = new ParsingLocation(_path, _charIndex, _lineNumber, _columnIndex)
  def atEOF: Boolean = _atEOF

  def error(msg: String): Nothing = throw new JParsingException(s"$msg at $location.", location)

  def peek(): Int = if(ensure(1)) _buffer(_bufferPos).toInt else -1
  def peek(offset: Int): Int = if(ensure(offset + 1)) _buffer(_bufferPos + offset).toInt else -1

  def ahead(c: Char): Boolean = ensure(1) && _buffer(_bufferPos) == c

  def ahead(str: String): Boolean = {
    @tailrec def loop(i: Int): Boolean =
      i == str.length || str.charAt(i) == _buffer(_bufferPos + i) && loop(i + 1)

    ensure(str.length) && loop(0)
  }

  def consume(c: Char): Boolean = ahead(c) && { drop(1); true }

  def consume(str: String): Boolean = ahead(str) && { drop(str.length); true }

  def require(c: Char): Boolean =
    if(consume(c)) true else error("Expected " + c.toString.literalEncode)

  def require(str: String): Boolean =
    if(consume(str)) true else error("Expected " + str.literalEncode)

  override def read(): Int = if(!ensure(1)) -1 else {
    val c = _buffer(_bufferPos).toInt
    drop(1)
    c
  }

  override def read(chars: Array[Char], offset: Int, length: Int): Int = {
    var wp = offset
    val end = offset + length
    while(wp < end && (_bufferPos < _bufferFill || fill(end - wp) > 0)) {
      chars(wp) = _buffer(_bufferPos)
      drop(1)
      wp += 1
    }
    wp - offset
  }

  def drop(n: Int): Unit = {
    val endBufferPos = _bufferPos + n
    while(_bufferPos < endBufferPos) {
      if('\n' == _buffer(_bufferPos)) {
        _lineNumber += 1
        _columnIndex = 0
      } else {
        _columnIndex += 1
      }
      _bufferPos += 1
    }
    _charIndex += n
  }

  /** Attempts to ensure that the buffer contains the requested number of characters. */
  private def ensure(requested: Int): Boolean = {
    val required = requested - (_bufferFill - _bufferPos)
    if(required <= 0) true else fill(required) >= required
  }

  /** Attempts to add the requested number of characters to the buffer, returning the
    * number of characters added. This may be less than the requested number if the
    * end of the input has been reached.
    */
  private def fill(requested: Int): Int = if(_atEOF) 0 else {
    if(_bufferPos != 0) {
      _bufferFill = _bufferFill - _bufferPos
      Array.copy(_buffer, _bufferPos, _buffer, 0, _bufferFill)
      _bufferPos = 0
    }

    // Here we stop spinning once we have the requested number of characters because
    // it does not make sense to wait on I/O when we know we can work
    var filled = 0
    if(_bufferFill < _buffer.length && !_atEOF) do {
      val n = reader.read(_buffer, _bufferFill, _buffer.length - _bufferFill)
      _bufferFill += n
      filled += n

      if(n <= 0) {
        _atEOF = true
      }
    } while(filled < requested && _bufferFill < _buffer.length && !_atEOF)

    filled
  }

  override def close(): Unit = {}
}
