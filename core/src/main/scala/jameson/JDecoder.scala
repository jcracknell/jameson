package jameson

trait JDecoder[+A] {
  def decode(reader: JReader): A
}
