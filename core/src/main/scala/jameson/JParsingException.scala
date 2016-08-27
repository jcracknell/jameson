package jameson

import jameson.dec.JParsingLocation

class JParsingException(
  msg: String,
  val location: Option[JParsingLocation]
) extends Exception(msg) {
  def this(msg: String) = this(msg, None)
  def this(msg: String, location: JParsingLocation) = this(msg, Some(location))
}
