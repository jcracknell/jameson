package jameson.examples

import jameson._
import org.scalatest.{FunSpec, Matchers}

class Reading extends FunSpec with Matchers {
  case class Company(name: String, url: java.net.URL, locations: Seq[Address])
  case class Address(lines: Seq[String], locality: String, region: String, postalCode: String, country: String)

  it("should read the example") {
    val reader = new java.io.StringReader("""
      {
        "@context": "http://www.schema.org",
        "@type": "Corporation",
        "name": "Lightbend, Inc.",
        "url": "https://www.lightbend.com",
        "address": { "@id": "primaryLocation" },
        "location": [
          {
            "@type": "PostalAddress",
            "@id": "primaryLocation",
            "streetAddress": ["625 Market Street", "10th Floor"],
            "addressLocality": "San Fransisco",
            "addressRegion": "CA",
            "postalCode": "94105",
            "addressCountry": "USA"
          }, {
            "@type": "PostalAddress",
            "streetAddress": ["3565 Piedmont Rd NE", "Suite 115"],
            "addressLocality": "Atlanta",
            "addressRegion": "GA",
            "postalCode": "30305",
            "addressCountry": "USA"
          }
        ]
      }
    """)

    Jameson.read(reader) {
      case j: JObjectReader =>
        val tpe = j.capture("@type") { case r: JStringReader => r.readAll() }
        val name = j.capture("name") { case r: JStringReader => r.readAll() }
        val url = j.capture("url") { case r: JStringReader => new java.net.URL(r.readAll()) }
        val locations = j.capture("location") {
          case j: JArrayReader => j collect {
            case j: JObjectReader =>
              val streetAddress = j.capture("streetAddress") {
                case j: JStringReader => Seq(j.readAll())
                case j: JArrayReader => j collect { case j: JStringReader => j.readAll() }
              }
              val addressLocality = j.capture("addressLocality") { case j: JStringReader => j.readAll() }
              val addressRegion = j.capture("addressRegion") { case j: JStringReader => j.readAll() }
              val addressCountry = j.capture("addressCountry") { case j: JStringReader => j.readAll() }
              val postalCode = j.capture("postalCode") { case j: JStringReader => j.readAll() }

              j.discard()

              Address(
                lines = streetAddress.get,
                locality = addressLocality.get,
                region = addressRegion.get,
                country = addressCountry.get,
                postalCode = postalCode.get
              )
          }
        }

        j.discard()

        Company(name.get, url.get, locations.get)

      case _ => throw new Exception("Expected object.")
    } should be (Company(
      name = "Lightbend, Inc.",
      url = new java.net.URL("https://www.lightbend.com"),
      locations = Seq(
        Address(
          lines = Seq("625 Market Street", "10th Floor"),
          locality = "San Fransisco",
          region = "CA",
          postalCode = "94105",
          country = "USA"
        ),
        Address(
          lines = Seq("3565 Piedmont Rd NE", "Suite 115"),
          locality = "Atlanta",
          region = "GA",
          postalCode = "30305",
          country = "USA"
        )
      )
    ))
  }
}
