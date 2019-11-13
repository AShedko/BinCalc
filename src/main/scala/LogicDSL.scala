import shapeless._

sealed trait FuncProp {
  def check[V<:ValueType,N<:Nat](v:V, f: Sized[Array[V],N] => V):Boolean = false
}

case class Commutative() extends FuncProp
case class Associative() extends FuncProp
case class HasIdentity() extends FuncProp

abstract class LogicDSL[_] {
  trait DSLFunction{}

  def Basis() : Map[Symbol, DSLFunction]
  def Laws() : Map[Symbol, FuncProp]
}

abstract class DSLExpr[DSL<:LogicDSL[_]]

abstract class Context[DSL<:LogicDSL[_]]

object DSLExpr{
  def Interpret[VT<:ValueType, DSL<:LogicDSL[VT]]: ((DSLExpr[DSL], Context[DSL]) => VT) = {
    ???
  }
}

object LogicDSL {}

object Main {
  def main(args: Array[String]): Unit = {

  }
}