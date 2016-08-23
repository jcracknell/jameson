import scala.language.implicitConversions

package object jameson {
  import scala.language.experimental.macros

  private[jameson] def using[A <: AutoCloseable, B](resource: A)(loan: A => B): B =
    macro jameson.macros.UsingMacro.apply[A, B]

  private[jameson] implicit def jamesonStringOps(str: String): jameson.util.JamesonStringOps = new jameson.util.JamesonStringOps(str)
}
