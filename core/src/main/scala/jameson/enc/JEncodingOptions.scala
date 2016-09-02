package jameson
package enc

trait JEncodingOptions {
  def arrayStyleAt(path: JPath, sizeHint: Int): JArrayStyle

  /** Indicates whether non lower-ASCII characters should always be escaped. */
  def escapeNonASCII: Boolean

  /** Indicates whether single quotes should be escaped in addition to double quotes. */
  def escapeQuotes: Boolean

  /** Indicates whether forward slashes should be escaped in addition to backslashes.
    * Useful when embedding JSON inside of `<script>` tags.
    */
  def escapeSlashes: Boolean

  def objectStyleAt(path: JPath, sizeHint: Int): JObjectStyle
}
