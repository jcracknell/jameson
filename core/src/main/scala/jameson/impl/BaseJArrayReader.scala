package jameson
package impl

abstract class BaseJArrayReader extends JArrayReader {
  private var closed = false
  private var consumed = false

  protected def foreach(f: JReader => Unit): Unit
  protected def newBuilder[A]: scala.collection.mutable.Builder[A, IndexedSeq[A]]

  def collect[A](collector: PartialFunction[JReader, A]): Seq[A] = {
    guard()

    val builder = newBuilder[A]
    for(v <- this) {
      if(collector.isDefinedAt(v))
        builder += collector(v)
    }
    builder.result()
  }

  def collectIndexed[A](collector: PartialFunction[(Int, JReader), A]): Seq[A] = {
    guard()

    var i = 0
    val builder = newBuilder[A]
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

    val builder = newBuilder[A]
    for(v <- this) {
      builder += projection(v)
    }
    builder.result()
  }

  def mapIndexed[A](projection: (Int, JReader) => A): IndexedSeq[A] = {
    guard()

    var i = 0
    val builder = newBuilder[A]
    for(v <- this) {
      builder += projection(i, v)
      i += 1
    }
    builder.result()
  }

  def partition[A](collector: PartialFunction[JReader, A]): (Seq[A], Seq[JValue]) = {
    guard()

    val matched = newBuilder[A]
    val unmatched = newBuilder[JValue]
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
    val matched = newBuilder[A]
    val unmatched = newBuilder[(Int, JValue)]
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
