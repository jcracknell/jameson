package jameson
package impl

abstract class BaseJArrayReader extends JArrayReader with AutoCloseable {
  private var closed = false
  private var consumed = false

  protected def foreach(f: (Int, JReader) => Unit): Unit

  protected def seqBuilder[A]: scala.collection.mutable.Builder[A, Seq[A]] = Vector.newBuilder[A]
  protected def indexedSeqBuilder[A]: scala.collection.mutable.Builder[A, IndexedSeq[A]] = Vector.newBuilder[A]

  def collect[A](collector: PartialFunction[JReader, A]): Seq[A] = {
    guard()

    val builder = seqBuilder[A]
    foreach { (i, valueReader) =>
      if(collector.isDefinedAt(valueReader)) {
        builder += collector(valueReader)
      } else {
        valueReader.discard()
      }
    }
    builder.result()
  }

  def collectIndexed[A](collector: PartialFunction[(Int, JReader), A]): Seq[A] = {
    guard()

    val builder = seqBuilder[A]
    foreach { (i, valueReader) =>
      val tup = (i, valueReader)
      if(collector.isDefinedAt(tup)) {
        builder += collector(tup)
      } else {
        valueReader.discard()
      }
    }
    builder.result()
  }

  def map[A](projection: JReader => A): IndexedSeq[A] = {
    guard()

    val builder = indexedSeqBuilder[A]
    foreach { (i, valueReader) =>
      builder += projection(valueReader)
    }
    builder.result()
  }

  def mapIndexed[A](projection: (Int, JReader) => A): IndexedSeq[A] = {
    guard()

    val builder = indexedSeqBuilder[A]
    foreach { (i, valueReader) =>
      builder += projection(i, valueReader)
    }
    builder.result()
  }

  def partition[A](collector: PartialFunction[JReader, A]): (Seq[A], Seq[JValue]) = {
    guard()

    val matched = seqBuilder[A]
    val unmatched = seqBuilder[JValue]
    foreach { (i, valueReader) =>
      if(collector.isDefinedAt(valueReader)) {
        matched += collector(valueReader)
      } else {
        unmatched += valueReader.copy()
      }
    }
    (matched.result(), unmatched.result())
  }

  def partitionIndexed[A](collector: PartialFunction[(Int, JReader), A]): (Seq[A], Seq[(Int, JValue)]) = {
    guard()

    val matched = seqBuilder[A]
    val unmatched = seqBuilder[(Int, JValue)]
    foreach { (i, valueReader) =>
      val tup = (i, valueReader)
      if(collector.isDefinedAt(tup)) {
        matched += collector(tup)
      } else {
        unmatched += ((i, valueReader.copy()))
      }
    }
    (matched.result(), unmatched.result())
  }

  def copy(): JArray = {
    guard()

    val builder = indexedSeqBuilder[JValue]
    foreach { (i, valueReader) =>
      builder += valueReader.copy()
    }

    JArray(builder.result())
  }

  def discard(): Unit = {
    guard()

    foreach { (_, valueReader) =>
      valueReader.discard()
    }
  }

  def close(): Unit = {
    if(!consumed) throw new UnsupportedOperationException("Attempted to close unconsumed JArrayReader.")

    closed = true
  }

  @inline protected def guard(): Unit = {
    if(closed) throw new UnsupportedOperationException("Attempted to access closed JArrayReader.")
    if(consumed) throw new UnsupportedOperationException("Attempted to access consumed JArrayReader.")

    consumed = true
  }
}
