package jameson
package enc

trait EncodingOptions extends JStringEncoder.Options {
  def arrayStyleAt(path: JPath, sizeHint: Int): JArrayStyle
  def objectStyleAt(path: JPath, sizeHint: Int): JObjectStyle
}
