package languageops

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

class FunctionSpec extends FlatSpec with Matchers with Checkers {

    "A Function" should "correctly calculate output with valid input" in {
        val f = Function.parse("(x: Int) => 5 - x % x").get
        f(17) should be (Some(5))
    }

    it should "return false on division by zero" in {
        val f = Function.parse("(x: Int) => 1 - x % x").get
        f(0) shouldBe empty
    }

    it should "handle integer overflows properly" in {
        val f = Function.parse("(x: Int) => x % 65535 - 2147483647").get
        f(-100) should be (Some(2147483549))
    }

    it should "return none if function is malformed" in {
        Function.parse("(x: Int) = 1 - x % x") shouldBe empty
    }

    it should "not accept arguments from (-inf, -100) U (100, inf)" in {
        val f = Function.parse("(x: Int) => 1 - x % 10").get
        f(-1000) shouldBe empty
        f(1000) shouldBe empty
    }

    it should "not accept undefined variables" in {
        Function.parse("(x: Int) => x - y % 10") should be (None)
    }

    it should "respect arithmetic function order" in {
        val f = Function.parse("(x: Int) => 4 - x % 2").get
        f(5) should be (Some(3))
    }

    "Functions of all valid classes" should "compute properly with valid inputs" in {
        val argumentNames = Gen.identifier.suchThat(!_.isEmpty)
        val argumentValues = Gen.choose(-100, 100)
        val nonZeroArgumentValues = argumentValues.suchThat(_ != 0)
        val constants = Gen.choose(1, 10)
        val functions = Map(
            "(%var%: Int) => %var% % %const1% - %const2%" -> ((x: Int, c1: Int, c2: Int) => x % c1 - c2),
            "(%var%: Int) => %const1% % %var% - %const2%" -> ((x: Int, c1: Int, c2: Int) => c1 % x - c2),
            "(%var%: Int) => %const1% % %const2% - %var%" -> ((x: Int, c1: Int, c2: Int) => c1 % c2 - x),
            "(%var%: Int) => %var% - %const1% % %const2%" -> ((x: Int, c1: Int, c2: Int) => x - c1 % c2),
            "(%var%: Int) => %const1% - %var% % %const2%" -> ((x: Int, c1: Int, c2: Int) => c1 - x % c2),
            "(%var%: Int) => %const1% - %const2% % %var%" -> ((x: Int, c1: Int, c2: Int) => c1 - c2 % x),
            "(%var%: Int) => %var% % %var% - %const1%" -> ((x: Int, c1: Int, c2: Int) => x % x - c1),
            "(%var%: Int) => %var% % %const1% - %var%" -> ((x: Int, c1: Int, c2: Int) => x % c1 - x),
            "(%var%: Int) => %const1% % %var% - %var%" -> ((x: Int, c1: Int, c2: Int) => c1 % x - x),
            "(%var%: Int) => %var% - %var% % %const1%" -> ((x: Int, c1: Int, c2: Int) => x - x % c1),
            "(%var%: Int) => %var% - %const1% % %var%" -> ((x: Int, c1: Int, c2: Int) => x - c1 % x),
            "(%var%: Int) => %const1% - %var% % %var%" -> ((x: Int, c1: Int, c2: Int) => c1 - x % x)
        )
        check(Prop.forAll(argumentNames, constants, constants, nonZeroArgumentValues, Gen.oneOf(functions.keys.toSeq)) {
            (arg: String, c1: Int, c2: Int, param: Int, functionTemplate: String) => {
                val functionText = functionTemplate
                    .replaceAll("%var%", arg)
                    .replaceAll("%const1%", c1.toString)
                    .replaceAll("%const2%", c2.toString)
                val testFunction = functions(functionTemplate)
                val f = Function.parse(functionText).get
                f(param) == Some(testFunction(param, c1, c2))
            }
        })
    }
}
