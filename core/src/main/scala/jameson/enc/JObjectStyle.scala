package jameson
package enc

import java.io.Writer

trait JObjectStyle {
  def at(path: JPath, sizeHint: Int): JObjectStyle
  def writeStart(writer: Writer): Unit
  def writeIndent(writer: Writer): Unit
  def writeAssign(writer: Writer): Unit
  def writeSeparator(writer: Writer): Unit
  def writeEnd(writer: Writer, size: Int): Unit
}

object JObjectStyle {
  object Compact extends JObjectStyle {
    def at(path: JPath, sizeHint: Int): JObjectStyle = this
    def writeStart(writer: Writer): Unit = writer.write('{')
    def writeIndent(writer: Writer): Unit = {}
    def writeAssign(writer: Writer): Unit = writer.write(':')
    def writeSeparator(writer: Writer): Unit = writer.write(',')
    def writeEnd(writer: Writer, size: Int): Unit = writer.write('}')
  }

  class Pretty protected (indent: String, newline: String, depth: Int) extends JObjectStyle {
    def this(indent: String = "  ", newline: String = "\n") = this(indent, newline, 0)

    override def at(path: JPath, sizeHint: Int): JObjectStyle =
      if(sizeHint <= 1) SingleLine else new Pretty(indent, newline, path.depth)

    def writeStart(writer: Writer): Unit = writer.write('{')

    def writeIndent(writer: Writer): Unit = writeIndent(writer, depth + 1)

    private def writeIndent(writer: Writer, indentCount: Int): Unit = {
      writer.write(newline)
      var i = 0
      while(i < indentCount) {
        writer.write(indent)
        i += 1
      }
    }

    def writeAssign(writer: Writer): Unit = writer.write(": ")

    def writeSeparator(writer: Writer): Unit = writer.write(',')

    def writeEnd(writer: Writer, size: Int): Unit = {
      if(size > 0) {
        writeIndent(writer, depth)
        writer.write('}')
      } else {
        writer.write(" }")
      }
    }
  }

  object SingleLine extends JObjectStyle {
    def at(path: JPath, sizeHint: Int): JObjectStyle = this
    def writeStart(writer: Writer): Unit = writer.write('{')
    def writeIndent(writer: Writer): Unit = writer.write(' ')
    def writeAssign(writer: Writer): Unit = writer.write(": ")
    def writeSeparator(writer: Writer): Unit = writer.write(',')
    def writeEnd(writer: Writer, size: Int): Unit = if(size > 0) writer.write(" }") else writer.write('}')
  }
}
