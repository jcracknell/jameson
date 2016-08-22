package jameson
package enc

import java.io.Writer

trait JObjectStyle {
  def at(path: JPath, sizeHint: Int): JObjectStyle
  def writeStart(writer: Writer): Unit
  def writeIndent(writer: Writer): Unit
  def writeAssign(writer: Writer): Unit
  def writeSeparator(writer: Writer): Unit
  def writeEnd(writer: Writer): Unit
}

object JObjectStyle {
  object Compact extends JObjectStyle {
    def at(path: JPath, sizeHint: Int): JObjectStyle = this
    def writeStart(writer: Writer): Unit = writer.write('{')
    def writeIndent(writer: Writer): Unit = {}
    def writeAssign(writer: Writer): Unit = writer.write(':')
    def writeSeparator(writer: Writer): Unit = writer.write(',')
    def writeEnd(writer: Writer): Unit = writer.write('}')
  }
}
