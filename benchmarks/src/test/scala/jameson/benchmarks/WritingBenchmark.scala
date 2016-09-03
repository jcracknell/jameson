package jameson.examples

object WritingBenchmark extends Benchmark {
  def iterations: Int = 50000

  benchmark("jameson") {
    import jameson._

    Jameson.writeObject { _
      .write("@context", "http://schema.org")
      .write("@type", "Invoice")
      .writeObject("broker") { _
        .write("@type", "LocalBusiness")
        .write("name", "ACME Home Heating")
      }
      .write("accountId", "xxxx-xxxx-xxxx-1234")
      .writeObject("customer") { _
        .write("@type", "Person")
        .write("name", "Jane Doe")
      }
      .write("paymentDueDate", "2015-01-30")
      .writeObject("minumumPaymentDue") { _
        .write("@type", "PriceSpecification")
        .write("price", 0.00d)
        .write("priceCurrency", "USD")
      }
      .writeObject("totalPaymentDue") { _
        .write("@type", "PriceSpecification")
        .write("price", 0.00d)
        .write("priceCurrency", "USD")
      }
      .write("paymentStatus", "http://schema.org/PaymentComplete")
      .writeArray("referencesOrder") { _
        .writeObject { _
          .write("@type", "Order")
          .write("description", "furnace")
          .write("orderDate", "2014-12-01")
          .write("orderNumber", "123ABC")
          .write("paymentMethod", "http://purl.org/goodrelations/v1#ByInvoice")
          .writeObject("orderedItem") { _
            .write("@type", "Product")
            .write("name", "ACME Furnace 3000")
            .write("productId", "ABC123")
          }
        }
        .writeObject { _
          .write("@type", "Order")
          .write("description", "furnace installation")
          .write("orderDate", "2014-12-02")
          .write("paymentMethod", "http://purl.org/goodrelations/v1#ByInvoice")
          .writeObject("orderedItem") { _
            .write("@type", "Service")
            .write("description", "furnace installation")
          }
        }
      }
    }
  }

  benchmark("play-json") {
    import play.api.libs.json._

    Json.stringify(Json.obj(
      "@context" -> "http://schema.org",
      "@type" -> "Invoice",
      "broker" -> Json.obj(
        "@type" -> "LocalBusiness",
        "name" -> "ACME Home Heating"
      ),
      "accountId" -> "xxxx-xxxx-xxxx-1234",
      "customer" -> Json.obj(
        "@type" -> "Person",
        "name" -> "Jane Doe"
      ),
      "paymentDueDate" -> "2015-01-30",
      "minimumPaymentDue" -> Json.obj(
        "@type" -> "PaymentSpecification",
        "price" -> 0.00d,
        "priceCurrency" -> "USD"
      ),
      "totalPaymentDue" -> Json.obj(
        "@type" -> "PaymentSpecification",
        "price" -> 0.00d,
        "priceCurrency" -> "USD"
      ),
      "paymentStatus" -> "http://schema.org/PaymentComplete",
      "referencesOrder" -> Json.arr(
        Json.obj(
          "@type" -> "Order",
          "description" -> "furnace",
          "orderDate" -> "2014-12-01",
          "orderNumber" -> "123ABC",
          "paymentMethod" -> "http://purl.org/goodrelations/v1#ByInvoice",
          "orderedItem" -> Json.obj(
            "@type" -> "Product",
            "name" -> "ACME Furnace 3000",
            "productId" -> "ABC123"
          )
        ),
        Json.obj(
          "@type" -> "Order",
          "description" -> "furnace installation",
          "orderDate" -> "2014-12-01",
          "orderNumber" -> "123ABC",
          "paymentMethod" -> "http://purl.org/goodrelations/v1#ByInvoice",
          "orderedItem" -> Json.obj(
            "@type" -> "Service",
            "description" -> "furnace installation"
          )
        )
      )
    ))
  }

  benchmark("spray-json") {
    import spray.json._

    JsObject(
      "@context" -> JsString("http://schema.org"),
      "@type" -> JsString("Invoice"),
      "broker" -> JsObject(
        "@type" -> JsString("LocalBusiness"),
        "name" -> JsString("ACME Home Heating")
      ),
      "accountId" -> JsString("xxxx-xxxx-xxxx-1234"),
      "customer" -> JsObject(
        "@type" -> JsString("Person"),
        "name" -> JsString("Jane Doe")
      ),
      "paymentDueDate" -> JsString("2015-01-30"),
      "minimumPaymentDue" -> JsObject(
        "@type" -> JsString("PriceSpecification"),
        "price" -> JsNumber(0.00d),
        "priceCurrency" -> JsString("USD")
      ),
      "totalPaymentDue" -> JsObject(
        "@type" -> JsString("PriceSpecification"),
        "price" -> JsNumber(0.00d),
        "priceCurrency" -> JsString("USD")
      ),
      "paymentStatus" -> JsString("http://schema.org/PaymentComplete"),
      "referencesOrder" -> JsArray(
        JsObject(
          "@type" -> JsString("Order"),
          "description" -> JsString("furnace"),
          "orderDate" -> JsString("2014-12-01"),
          "orderNumber" -> JsString("123ABC"),
          "paymentMethod" -> JsString("http://purl.org/goodrelations/v1#ByInvoice"),
          "orderedItem" -> JsObject(
            "@type" -> JsString("Product"),
            "name" -> JsString("ACME Furnace 3000"),
            "productId" -> JsString("ABC123")
          )
        ),
        JsObject(
          "@type" -> JsString("Order"),
          "description" -> JsString("furnace installation"),
          "orderDate" -> JsString("2014-12-02"),
          "paymentMethod" -> JsString("http://purl.org/goodrelations/v1#ByInvoice"),
          "orderedItem" -> JsObject(
            "@type" -> JsString("Service"),
            "description" -> JsString("furnace installation")
          )
        )
      )
    ).compactPrint
  }
}
