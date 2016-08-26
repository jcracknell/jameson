package jameson
package enc

import java.io.Writer

trait JArrayStyle {
  def at(path: JPath, sizeHint: Int): JArrayStyle
  def writeStart(writer: Writer): Unit
  def writeIndent(writer: Writer): Unit
  def writeSeparator(writer: Writer): Unit
  def writeEnd(writer: Writer, size: Int): Unit
}

object JArrayStyle {
  object Compact extends JArrayStyle {
    def at(path: JPath, sizeHint: Int): JArrayStyle = this
    def writeStart(writer: Writer): Unit = writer.write('[')
    def writeIndent(writer: Writer): Unit = {}
    def writeSeparator(writer: Writer): Unit = writer.write(',')
    def writeEnd(writer: Writer, size: Int): Unit = writer.write(']')
  }
}
