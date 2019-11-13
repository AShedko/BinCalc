import org.scalatest.FunSpec
import Expression._

class ExpressionTest extends FunSpec {
  describe("An expression") {
    it ("should evaluate correctly") {
      val expr = Or(
        And(Constant(true),Constant(false)),
        PlusBig(List(Constant(true), Variable(2), Variable(1), Variable(3)))
      )
      assert(! simpleEval(expr, Map(
        (1, true), (2, true), (3, true)
      ))
      )
    }

    val expr2 = Or(Constant(false),Variable(1))
    it("should be possible to partially evaluate") {
      assert(partialEval(expr2, Map((1,true))) == Constant(true))
    }
    it ("'s evaluation should not change the object"){
      assert(usedVariables(expr2).nonEmpty)
    }
    it ("evaluation's result should not have variables if all are substituted into"){
      assert(usedVariables(partialEval(expr2, Map((1,true)))).isEmpty)
    }
  }
}