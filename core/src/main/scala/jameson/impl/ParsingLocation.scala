package jameson
package impl

/** Represents a location within a stream of characters.
  *
  * @param path The path in the object graph.
  * @param charIndex The 0-based character index within the input stream.
  * @param lineNumber The 1-based line index within the input stream.
  * @param columnIndex The 0-base character index within the line.
  */
class ParsingLocation(
  val path: JPath,
  val charIndex: Int,
  val lineNumber: Int,
  val columnIndex: Int
) {
  override def hashCode(): Int = {
    var hash = path.hashCode()
    hash = scala.runtime.Statics.mix(hash, charIndex)
    hash = scala.runtime.Statics.mix(hash, lineNumber)
    scala.runtime.Statics.mixLast(hash, columnIndex)
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: ParsingLocation =>
      this.path == that.path &&
      this.charIndex == that.charIndex &&
      this.lineNumber == that.lineNumber &&
      this.columnIndex == that.columnIndex
    case _ => false
  }

  override def toString: String = s"$path (L${lineNumber}C${columnIndex})"
}
