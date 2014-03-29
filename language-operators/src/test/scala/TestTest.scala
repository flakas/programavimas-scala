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

    test("'var % const - const' class function computes properly") {
        val argumentNames = Gen.identifier.suchThat(_ != "")
        val argumentValues = Gen.choose(-100, 100)
        check(Prop.forAll(argumentNames, argumentValues) {
            (arg: String, param: Int) => {
                val func = Function.parse("(" + arg + ": Int) => " + arg + " % 101 - 0")
                (func.flatMap((f: (Int => Option[Int])) => f(param))) == Some(param)
            }
        })
    }
}
