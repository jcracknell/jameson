package jameson
package enc

trait EncodingOptions extends JStringEncoder.Options {
  def indent: String
  def newline: String
  def sep: String
}
