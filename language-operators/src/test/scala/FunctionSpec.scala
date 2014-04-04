package languageops

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Arbitrary._
//import org.scalacheck.Prop.{forAll, BooleanOperators}

class FunctionSpec extends FlatSpec with Matchers with Checkers {

    class NonZeroArgument(x: Int) {
        def get = x
    }

    implicit lazy val nonZeroArgumentGenerator: Arbitrary[NonZeroArgument] = Arbitrary {
        for {
            c <- Gen.choose(-100, 100).suchThat(_ != 0)
        } yield new NonZeroArgument(c)
    }

    implicit lazy val constantGenerator: Arbitrary[Constant] = Arbitrary {
        for {
            c <- Gen.choose(1, 10)
        } yield Constant(c.toString)
    }

    implicit lazy val functionGenerator: Arbitrary[Op] = Arbitrary {
        for {
            operationOrder <- arbitrary[Boolean]
            constants <- Gen.containerOfN[List, Constant](3, arbitrary[Constant])
            operands <- Gen.listOfN(3, arbitrary[Boolean]).suchThat((x: List[Boolean]) => x.contains(true) && x.contains(false))
            ops = operands.zip(constants).map((x: (Boolean, Constant)) => if (x._1) Var("x") else x._2)
        } yield
            if (operationOrder) {
                Subtract(Modulo(ops(0), ops(1)), ops(2))
            } else {
                Subtract(ops(0), Modulo(ops(1), ops(2)))
            }
    }

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
        check(Prop.forAll {
            (func: Op, param: NonZeroArgument) => {
                val f = Function.parse("(x: Int) => " + func.toString).get
                f(param.get) == Some(func.compute(param.get))
            }
        })
    }
}
