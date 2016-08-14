package jameson
package impl


abstract class BaseJObjectReader extends JObjectReader with AutoCloseable {
  private var closed = false
  private var consumed = false

  protected def foreach(f: (String, JReader) => Unit): Unit

  protected def seqBuilder[A]: scala.collection.mutable.Builder[A, Seq[A]] = Vector.newBuilder[A]

  def collect[A](collector: PartialFunction[(String, JReader), A]): Seq[A] = {
    guard()

    val builder = seqBuilder[A]
    foreach { (name, reader) =>
      val tup = (name, reader)

      if(collector.isDefinedAt(tup))
        builder += collector(tup)
    }

    builder.result()
  }

  override def copy(): JObject = {
    guard()

    val builder = JObject.builder
    foreach { (name, reader) =>
      builder.add((name, reader.copy()))
    }

    builder.result
  }


  /** Consumes the reader, discarding any further content. */
  def discard(): Unit = {
    guard()

    foreach { (_, reader) => reader.discard() }
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

    consumed = true
  }
}
