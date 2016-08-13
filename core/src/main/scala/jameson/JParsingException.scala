package jameson

import jameson.impl.ParsingLocation

class JParsingException(
  msg: String,
  val location: Option[ParsingLocation]
) extends Exception(msg) {
  def this(msg: String) = this(msg, None)
  def this(msg: String, location: ParsingLocation) = this(msg, Some(location))
}
