package jameson
package macros

import scala.reflect.macros.blackbox.Context

class UsingMacro(val c: Context) {
  import c.universe._

  def apply[A <: AutoCloseable : WeakTypeTag, B : WeakTypeTag](resource: Expr[A])(loan: Expr[A => B]): Expr[B] = {
    val ex = TermName(c.freshName("ex"))
    val res = TermName(c.freshName("res"))

    val loanAction = loan.tree match {
      case Block(Nil, Function(param :: Nil, body)) if canInline(body) => substitute(body, param.symbol, Ident(res))
      case Function(param :: Nil, body) if canInline(body) => substitute(body, param.symbol, Ident(res))
      case _ => q"(${loan.tree}).apply($res)"
    }

    c.Expr[B] {
      q"""{
        var $ex: java.lang.Throwable = null
        val $res = $resource
        try { $loanAction } catch {
          case t: java.lang.Throwable => $ex = t; throw t
        } finally {
          if($ex != null)
            try { $res.close() } catch { case t: java.lang.Throwable => $ex.addSuppressed(t) }
          else
            $res.close()
        }
      }"""
    }
  }

  def substitute(tree: Tree, from: Symbol, to: Tree): Tree =
    c.untypecheck(new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case _: Ident if tree.symbol == from => to
        case _ => super.transform(tree)
      }
    }.transform(tree))

  def canInline(tree: Tree): Boolean = {
    var result = true
    new Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case _: Return => result = false
        case _: UnApply => result = false
        case _ => super.traverse(tree)
      }
    }.traverse(tree)
    result
  }
}
