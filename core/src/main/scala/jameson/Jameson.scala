package jameson

import jameson.dec._
import java.io.Reader

object Jameson extends jameson.enc.EncodingApi {
  def decode[A](str: String)(loan: JReader => A): A =
    decode(new java.io.StringReader(str))(loan)

  def decode[A](reader: Reader)(loan: JReader => A): A = {
    val ctx = new ParsingContext(reader, JPath)
    Parser.parse(ctx)(loan)
  }
}
