package object jameson {
  // TODO: macro
  private[jameson] def using[A <: AutoCloseable, B](resource: A)(loan: A => B): B = {
    var thrown: Throwable = null
    try loan(resource)
    catch { case t: Throwable => thrown = t; throw t }
    finally {
      if(thrown == null) resource.close() else {
        try resource.close()
        catch { case t: Throwable => thrown.addSuppressed(t) }
      }
    }
  }
}
