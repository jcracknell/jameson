# jameson

Yet another scala API for JSON processing, featuring:

  - Fully streaming parsing and encoding process with single character lookahead
  - Idiomatic API making extensive use of the loan pattern
  - Ordered object keys
  - Zero dependencies

## Reading

    import jameson._

    val result = Jameson.read(reader: java.io.Reader) {
      // Scalar JSON values are automatically marshaled to their DOM representation
      case JNull => null
      case JBoolean(bool) => bool
      case JNumber(dbl) => dbl

      // Strings are read using JStringReader extends java.io.Reader
      case s: JStringReader => s.readAll()
      
      case a: JArrayReader => a collect {
        case JNumber(d) => d
        case _ => throw new Exception("Expected number in array at ${a.path}")
      }

      case o: JObjectReader =>
        val name = o.capture("name") { case s: JStringReader => s.readAll() }
        val age = o.capture("age") { case JDouble(n) => n }
        val birthDate = o.capture("birthDate") { case s: JStringReader => java.time.LocalDate.parse(s.readAll()) }

        // Fill captures discarding uncaptured properties
        o.discard()

        new Person(name.get, age.get, birthDate.get)
    }
