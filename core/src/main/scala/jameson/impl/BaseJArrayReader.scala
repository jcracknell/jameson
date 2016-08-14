package jameson
package impl

abstract class BaseJArrayReader extends JArrayReader with AutoCloseable {
  private var closed = false
  private var consumed = false

  protected def foreach(f: JReader => Unit): Unit

  protected def seqBuilder[A]: scala.collection.mutable.Builder[A, Seq[A]] = Vector.newBuilder[A]
  protected def indexedSeqBuilder[A]: scala.collection.mutable.Builder[A, IndexedSeq[A]] = Vector.newBuilder[A]

  def collect[A](collector: PartialFunction[JReader, A]): Seq[A] = {
    guard()

    val builder = seqBuilder[A]
    for(v <- this) {
      if(collector.isDefinedAt(v))
        builder += collector(v)
    }
    builder.result()
  }

  def collectIndexed[A](collector: PartialFunction[(Int, JReader), A]): Seq[A] = {
    guard()

    var i = 0
    val builder = seqBuilder[A]
    for(v <- this) {
      val tup = (i, v)
      if(collector.isDefinedAt(tup)) {
        builder += collector(tup)
      }
      i += 1
    }
    builder.result()
  }

  def map[A](projection: JReader => A): IndexedSeq[A] = {
    guard()

    val builder = indexedSeqBuilder[A]
    for(v <- this) {
      builder += projection(v)
    }
    builder.result()
  }

  def mapIndexed[A](projection: (Int, JReader) => A): IndexedSeq[A] = {
    guard()

    var i = 0
    val builder = indexedSeqBuilder[A]
    for(v <- this) {
      builder += projection(i, v)
      i += 1
    }
    builder.result()
  }

  def partition[A](collector: PartialFunction[JReader, A]): (Seq[A], Seq[JValue]) = {
    guard()

    val matched = seqBuilder[A]
    val unmatched = seqBuilder[JValue]
    for(v <- this) {
      if(collector.isDefinedAt(v)) {
        matched += collector(v)
      } else {
        unmatched += v.copy()
      }
    }
    (matched.result(), unmatched.result())
  }

  def partitionIndexed[A](collector: PartialFunction[(Int, JReader), A]): (Seq[A], Seq[(Int, JValue)]) = {
    guard()

    var i = 0
    val matched = seqBuilder[A]
    val unmatched = seqBuilder[(Int, JValue)]
    for(v <- this) {
      val tup = (i, v)
      if(collector.isDefinedAt(tup)) {
        matched += collector(tup)
      } else {
        unmatched += ((i, v.copy()))
      }
      i += 1
    }
    (matched.result(), unmatched.result())
  }

  def copy(): JArray = {
    guard()

    val builder = indexedSeqBuilder[JValue]
    foreach { reader =>
      builder += reader.copy()
    }
    JArray(builder.result())
  }

  def discard(): Unit = {
    guard()

    foreach(_.discard())
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
