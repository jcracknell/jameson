package jameson
package dec

abstract class BaseJObjectReader extends JObjectReader with AutoCloseable { objectReader =>
  private var closed = false
  private var consumed = false
  private var captures = Map.empty[String, BaseCapture[_]]

  protected def seqBuilder[A]: scala.collection.mutable.Builder[A, Seq[A]] = Vector.newBuilder[A]

  protected def consume(f: (String, JReader) => Unit): Unit

  protected def consumeUncaptured(uncaptured: (String, JReader) => Unit): Unit = {
    consumed = true

    if(captures.isEmpty) consume(uncaptured) else consume { (name, reader) =>
      captures.get(name) match {
        case Some(capture) => capture.fillFrom(reader)
        case None => uncaptured(name, reader)
      }
    }
  }

  protected def registerCapture[A](capture: BaseCapture[A]): BaseCapture[A] = {
    guard()

    if(captures.contains(capture.name))
      throw new IllegalStateException(s"Attempt to register duplicate capture for ${capture.path}.")

    captures += ((capture.name, capture))
    capture
  }

  def capture[A](name: String)(collector: PartialFunction[JReader, A]): JObjectReader.Capture[A] =
    registerCapture(new PartialCapture(path / name, collector))

  def captureValue(name: String): JObjectReader.Capture[JValue] =
    registerCapture(new ValueCapture(path / name))

  def collect[A](collector: PartialFunction[(String, JReader), A]): Seq[A] = {
    guard()

    val builder = seqBuilder[A]
    consumeUncaptured { (name, reader) =>
      val tup = (name, reader)

      if(collector.isDefinedAt(tup)) {
        builder += collector(tup)
      } else {
        reader.discard()
      }
    }

    builder.result()
  }

  def collectAll[A](collector: PartialFunction[(String, JReader), A]): (Seq[A], Seq[(String, JValue)]) = {
    guard()

    val matched = seqBuilder[A]
    val unmatched = seqBuilder[(String, JValue)]
    consumeUncaptured { (name, reader) =>
      val tup = (name, reader)
      if(collector.isDefinedAt(tup)) {
        matched += collector(tup)
      } else {
        unmatched += ((name, reader.copy()))
      }
    }

    (matched.result(), unmatched.result())
  }

  override def copy(): JObject = {
    guard()

    val builder = JObject.builder
    consumeUncaptured { (name, reader) =>
      builder.add((name, reader.copy()))
    }

    builder.result
  }


  /** Consumes the reader, discarding any further content. */
  def discard(): Unit = {
    guard()

    consumeUncaptured { (_, reader) => reader.discard() }
  }

  def close(): Unit = {
    if(!consumed)
      throw new UnsupportedOperationException("Attempted to close unconsumed JObjectReader.")

    closed = true
  }

  @inline protected def guard(): Unit = {
    if(closed)
      throw new UnsupportedOperationException("Attempted to access closed JObjectReader.")
    if(consumed)
      throw new UnsupportedOperationException("Attempted to access consumed JObjectReader.")
  }

  protected trait BaseCapture[+A] extends JObjectReader.Capture[A] {
    protected def capturedValue: Any

    def fillFrom(reader: JReader): Unit

    def get: A = {
      assertConsumed()

      if(!wasCaptured)
        throw new IllegalStateException(s"Attempt to access uncaptured value of $this")

      capturedValue.asInstanceOf[A]
    }

    @inline protected def assertConsumed(): Unit = {
      if(!objectReader.consumed)
        throw new IllegalStateException(s"Attempt to access $this for unconsumed reader.")
    }
  }

  protected class ValueCapture(val path: JPath.Property) extends BaseCapture[JValue] {
    protected var capturedValue: Any = _
    private var present = false

    def fillFrom(reader: JReader): Unit = {
      present = true
      capturedValue = reader.copy()
    }

    def wasCaptured: Boolean = {
      assertConsumed()
      present
    }

    def wasPresent: Boolean = {
      assertConsumed()
      present
    }
  }

  protected class PartialCapture[+A](val path: JPath.Property, collector: PartialFunction[JReader, A]) extends BaseCapture[A] {
    protected var capturedValue: Any = _
    private var present = false
    private var captured = false

    def fillFrom(reader: JReader): Unit = {
      present = true
      if(collector.isDefinedAt(reader)) {
        capturedValue = collector(reader)
        captured = true
      }
    }

    def wasCaptured: Boolean = {
      assertConsumed()
      captured
    }

    def wasPresent: Boolean = {
      assertConsumed()
      present
    }
  }
}
