package jameson
package enc

import java.io.Writer

import jameson.JPath

final class EncodingContext(val writer: Writer, var path: JPath, val options: EncodingOptions) {
  def error(msg: String): Nothing = throw new Exception(s"$msg at $path.")
}
