sealed trait Expression

sealed trait Value extends Expression

case class Constant(v: Boolean) extends Expression with Value
case class Variable(id: Int) extends Expression with Value

sealed trait Operation extends Expression

trait Unary extends Expression with Operation

case class Not(Arg: Expression) extends Operation with Unary

sealed trait Binary extends Expression with Operation

case class And(l: Expression, r: Expression) extends Expression with Binary
case class Or(l: Expression, r: Expression) extends Expression with Binary
case class Plus(l: Expression, r: Expression) extends Expression with Binary

sealed trait NAry extends Expression with Operation

case class AndBig(args: List[Expression]) extends Expression with NAry
case class OrBig(args: List[Expression]) extends Expression with NAry
case class PlusBig(args: List[Expression]) extends Expression with NAry

object Expression {
  def simpleEval(expression: Expression, args: Map[Int, Boolean]): Boolean = {
    expression match {
      case Constant(c) => c
      case Variable(i) => args(i)
      case Not(v) => !(simpleEval(v, args))
      case And(l, r) => simpleEval(l, args) && simpleEval(r, args)
      case Or(l, r) => simpleEval(l, args) || simpleEval(r, args)
      case Plus(l, r) => simpleEval(l, args) ^ simpleEval(r, args)
      case AndBig(as) => as.map(x => simpleEval(x, args)).forall(b => b)
      case OrBig(as) => !(as.map(x => !(simpleEval(x, args))).forall(b => b))
      case PlusBig(as) => as.map(x => (simpleEval(x, args))).
        fold(false)((a: Boolean, b: Boolean) => (a ^ b))
    }
  }

  def usedVariables(expression: Expression): Set[Int] = {
    val rec_big_op = { args: List[Expression] => args.map(a => usedVariables(a)).fold(Set())((a, b) => a ++ b) }
    expression match {
      case Constant(v) => Set()
      case Variable(id) => Set(id)
      case Not(v) => usedVariables(v)
      case And(l, r) => usedVariables(l) ++ usedVariables(r)
      case Or(l, r) => usedVariables(l) ++ usedVariables(r)
      case Plus(l, r) => usedVariables(l) ++ usedVariables(r)
      case AndBig(args) => rec_big_op(args)
      case OrBig(args) => rec_big_op(args)
      case PlusBig(args) => rec_big_op(args)
    }
  }

  def constantPropagate(expression: Expression):Expression = {
    expression match {
      case Not(Constant(c)) => Constant(!c)
      case Not(Not(v)) => constantPropagate(v) // Early optimization
      case And(Constant(false), r) => Constant(false)
      case And(l, Constant(false)) => Constant(false)
      case And(Constant(true), r) => r
      case And(l, Constant(true)) => l
      case And(l, r) => And(constantPropagate(l), constantPropagate(r))
      case Or(Constant(true), r) => Constant(true)
      case Or(l, Constant(true)) => Constant(true)
      case Or(Constant(false), r) => r
      case Or(l, Constant(false)) => l
      case Or(l, r) => Or(constantPropagate(l), constantPropagate(r))
      case Plus(l,r) if (l == r)  => Constant(false)
      case Plus(l,Constant(false)) => l
      case Plus(Constant(false),r) => r
      case Plus(l,Constant(true)) => Not(l)
      case Plus(Constant(true),r) => Not(r)
      case Plus(l,r) => Plus(constantPropagate(l), constantPropagate(r))
      case AndBig(args) =>
        if (!args.forall {
          case Constant(false) => false
          case _ => true
        }) {
          Constant(false)
        } else {
          AndBig(args.filter({
            case Constant(true) => false
            case _ => true
          }).map(constantPropagate))
        }
      case OrBig(args) =>
        if (!args.forall {
          case Constant(true) => false
          case _ => true
        }) {
          Constant(true)
        } else {
          OrBig(args.filter({
            case Constant(false) => false
            case _ => true
          }).map(constantPropagate))
        }
      case PlusBig(args) => {
        // Imperative hax begin
        var count_trues = 0
        var args2 = args.filter({
         case Constant(false) => false
         case Constant(true) => count_trues += 1; false
         case _ => true
       })
        if (count_trues % 2 == 1) {
          args2 = args2.appended(Constant(true))
        }
        // Imperative hax end
        PlusBig(args2.map(constantPropagate))
      }
      case other_expr => other_expr
    }
  }

  def partialEval(expression: Expression, args: Map[Int, Boolean]): Expression = {
    val substituted = expression match {
      case Constant(c) => Constant(c)
      case Variable(i) => args.get(i) match {
        case Some(b) => Constant(b)
        case None => Variable(i)
      }
      case Not(v) => Not(partialEval(v, args))
      case And(l, r) => And(partialEval(l, args), partialEval(r, args))
      case Or(l, r) => Or(partialEval(l, args), partialEval(r, args))
      case Plus(l, r) => Plus(partialEval(l, args), partialEval(r, args))
      case AndBig(as) => AndBig(as.map(x => partialEval(x, args)))
      case OrBig(as) => OrBig(as.map(x => (partialEval(x, args))))
      case PlusBig(as) => PlusBig(as.map(x => (partialEval(x, args))))
    }
    constantPropagate(substituted)
  }

  def intersp[A](l:List[A], what:A ): List[A] =
    l match {
      case ::(head, next) => head :: what :: intersp(next, what)
      case Nil => Nil
    }

  def prettyPrint(expression: Expression): String = {
    def splat(args: List[Expression]): String = {
      val s = args.map(prettyPrint).foldRight("")((a,b)=>a+","+b)
      s.substring(0, s.length-1)
    }
    expression match {
      case Constant(v) => s"$v"
      case Variable(id) => s"x$id"
      case Not(v) => s"¬($v)"
      case And(l, r) => s"(${prettyPrint(l)})∧(${prettyPrint(r)})"
      case Or(l, r) => s"(${prettyPrint(l)})∨(${prettyPrint(r)})"
      case Plus(l, r) => s"(${prettyPrint(l)})⊕(${prettyPrint(r)})"
      case AndBig(args) => s"⋀(${splat(args)})"
      case OrBig(args) => s"⋁(${splat(args)})"
      case PlusBig(args) =>s"⨁(${splat(args)})"
    }
  }
  def parse(string: String):Expression = {
    Constant(false)
  }
}