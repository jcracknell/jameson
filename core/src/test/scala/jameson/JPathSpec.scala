package jameson

import org.scalatest.{FunSpec, Matchers}

class JPathSpec extends FunSpec with Matchers {
  describe("toString") {
    it("should correctly represent the base path") {
      JPath.Base.toString should be (".")
    }
    it("should correctly represent indexes") {
      JPath(_/0).toString should be (".[0]")
      JPath(_/1).toString should be (".[1]")
    }
    it("should correctly represent properties") {
      JPath(_/"a").toString should be (""".["a"]""")
      JPath(_/"foo").toString should be (""".["foo"]""")
    }
    it("should correctly represent mixed paths") {
      JPath(_/"foo"/0/"baz").toString should be (""".["foo"][0]["baz"]""")
    }
  }
}
