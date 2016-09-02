package jameson
package util

class JamesonStringOps(private val self: String) extends AnyVal {
  /** Encodes the subject string as a JSON string literal. */
  def literalEncode: String = jameson.enc.EncodingJValueWriter.encodeString(self)
}
