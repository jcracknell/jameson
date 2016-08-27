package jameson
package enc

trait JEncodingOptions extends EncodingJStringWriter.Options {
  def arrayStyleAt(path: JPath, sizeHint: Int): JArrayStyle
  def objectStyleAt(path: JPath, sizeHint: Int): JObjectStyle
}
