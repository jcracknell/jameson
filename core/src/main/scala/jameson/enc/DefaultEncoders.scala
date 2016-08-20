package jameson
package enc

trait DefaultEncoders extends LowPriorityEncoders {
  implicit object ofNull extends JEncoder[Null] {
    def encode(a: Null, writer: JValueWriter): Unit = writer.writeNull()
  }

  implicit object ofBoolean extends JEncoder[Boolean] {
    def encode(a: Boolean, writer: JValueWriter): Unit = writer.writeBoolean(a)
  }

  implicit object ofDouble extends JEncoder[Double] {
    def encode(a: Double, writer: JValueWriter): Unit = writer.writeNumber(a)
  }

  implicit object ofFloat extends JEncoder[Float] {
    def encode(a: Float, writer: JValueWriter): Unit = writer.writeNumber(a.toDouble)
  }

  implicit object ofInt extends JEncoder[Int] {
    def encode(a: Int, writer: JValueWriter): Unit = writer.writeNumber(a.toDouble)
  }

  implicit object ofShort extends JEncoder[Short] {
    def encode(a: Short, writer: JValueWriter): Unit = writer.writeNumber(a.toDouble)
  }

  implicit object ofString extends JEncoder[String] {
    def encode(a: String, writer: JValueWriter): Unit = writer.writeString(a)
  }

  implicit object ofJNull extends JEncoder[JNull.type] {
    def encode(a: JNull.type, writer: JValueWriter): Unit = writer.writeNull()
  }

  implicit object ofJNumber extends JEncoder[JNumber] {
    def encode(a: JNumber, writer: JValueWriter): Unit = writer.writeNumber(a)
  }

  implicit def ofJBoolean[A <: JBoolean]: JEncoder[A] = _ofJBoolean.asInstanceOf[JEncoder[A]]
  private object _ofJBoolean extends JEncoder[JBoolean] {
    def encode(a: JBoolean, writer: JValueWriter): Unit = writer.writeBoolean(a)
  }

  implicit object ofJString extends JEncoder[JString] {
    def encode(a: JString, writer: JValueWriter): Unit = writer.writeString(a)
  }

  implicit object ofJArray extends JEncoder[JArray] {
    def encode(a: JArray, writer: JValueWriter): Unit = writer.writeArray(a)
  }

  implicit object ofJObject extends JEncoder[JObject] {
    def encode(a: JObject, writer: JValueWriter): Unit = writer.writeObject(a)
  }

  implicit def ofSeq[A](implicit encoder: JEncoder[A]): JEncoder[Seq[A]] = new JEncoder[Seq[A]] {
    def encode(a: Seq[A], writer: JValueWriter): Unit =
      writer writeArray { arrayWriter =>
        a foreach { el => arrayWriter.write(el) }
      }
  }

  implicit def ofArray[A](implicit encoder: JEncoder[A]): JEncoder[Array[A]] = new JEncoder[Array[A]] {
    override def encode(a: Array[A], writer: JValueWriter): Unit =
      writer writeArray { arrayWriter =>
        var i = 0
        while(i < a.length) {
          arrayWriter.write(a(i))
          i += 1
        }
      }
  }

  implicit def ofMap[A](implicit encoder: JEncoder[A]): JEncoder[Map[String, A]] = new JEncoder[Map[String, A]] {
    def encode(a: Map[String, A], writer: JValueWriter): Unit =
      writer writeObject { objectWriter =>
        a foreach { case (name, value) => objectWriter.write(name, value) }
      }
  }
}

trait LowPriorityEncoders { _: DefaultEncoders =>
  implicit object ofJValue extends JEncoder[JValue] {
    def encode(a: JValue, writer: JValueWriter): Unit = a match {
      case a: JString => writer.writeString(a)
      case a: JNumber => writer.writeNumber(a)
      case a: JObject => writer.writeObject(a)
      case a: JArray => writer.writeArray(a)
      case a: JBoolean => writer.writeBoolean(a)
      case JNull => writer.writeNull()
    }
  }
}
