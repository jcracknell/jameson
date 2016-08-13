package jameson

import org.scalatest.{FunSpec, Matchers}

class JPathSpec extends FunSpec with Matchers {
  describe("toString") {
    it("should correctly represent the relative base path") {
      JPath.relative.toString should be (".")
    }
    it("should correctly represent the unknown base path") {
      JPath.stream.toString should be ("<?>#.")
    }
    it("should correctly represent a uri base path") {
      JPath.uri("https://github.com/jcracknell/jameson").toString should be ("https://github.com/jcracknell/jameson#.")
    }
  }
  describe("pathString") {
    it("should correctly represent indexes") {
      (JPath.relative/0).pathString should be (".[0]")
      (JPath.relative/1).pathString should be (".[1]")
    }
    it("should correctly represent properties") {
      (JPath.relative/"a").pathString should be (""".["a"]""")
      (JPath.relative/"foo").pathString should be (""".["foo"]""")
    }
    it("should correctly represent mixed paths") {
      (JPath.relative/"foo"/0/"baz").pathString should be (""".["foo"][0]["baz"]""")
    }
  }
}
