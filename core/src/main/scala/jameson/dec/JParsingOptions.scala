package jameson.dec

trait JParsingOptions {
  /** Allow Javascript comments to appear in whitespace. */
  def allowComments: Boolean
  /** Allow strings delimited by single quotes. */
  def allowSingleQuotes: Boolean
}
