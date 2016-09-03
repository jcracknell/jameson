package jameson.examples

object WritingBenchmark extends Benchmark {
  benchmark(50000)("jameson") {
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
  benchmark(50000)("spray-json") {
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
