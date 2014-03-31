package languageops

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop.{forAll, BooleanOperators}

class TestFunction extends FunSuite with Checkers {
    test("correctly calculates function output") {
        expect(Some(5)) {
            val func = Function.parse("(x: Int) => 5 - x % x")
            func.flatMap((f: (Int => Option[Int])) => f(17))
        }
        expect(Some(-6)) {
            val func = Function.parse("(x: Int) => 1 - x % 10")
            func.flatMap((f: (Int => Option[Int])) => f(17))
        }
    }

    test("division by zero returns false") {
        expect(None) {
            val func = Function.parse("(x: Int) => 1 - x % x")
            func.flatMap((f: (Int => Option[Int])) => f(0))
        }
    }

    test("negative integer overflows are handled properly") {
        expect(Some(2147483549)) {
            val func = Function.parse("(x: Int) => x % 65535 - 2147483647")
            func.flatMap((f: (Int => Option[Int])) => f(-100))
        }
    }

    test("malformed function (missing `=>`) fails") {
        expect(None) { Function.parse("(x: Int) = 1 - x % x") }
    }

    test("does not accept arguments from (-inf, -100) U (100, inf)") {
        expect(None) {
            val func = Function.parse("(x: Int) => 1 - x % 10")
            func.flatMap((f: (Int => Option[Int])) => f(-1000))
        }
        expect(None) {
            val func = Function.parse("(x: Int) => 1 - x % 10")
            func.flatMap((f: (Int => Option[Int])) => f(1000))
        }
    }

    test("verifies that no other variable names are used") {
        expect(None) { Function.parse("(x: Int) => x - y % 10") }
    }

    test("arithmetic function order is correct") {
        expect(Some(3)) {
            Function.parse("(x: Int) => 4 - x % 2").flatMap((f: (Int => Option[Int])) => f(5))
        }
    }

    test("Functions of all valid classes compute properly with valid inputs") {
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
                val func = Function.parse(functionText)
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(testFunction(param, c1, c2))
            }
        })
    }
}
