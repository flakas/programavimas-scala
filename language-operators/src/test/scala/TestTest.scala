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

    val argumentNames = Gen.identifier.suchThat(!_.isEmpty)
    val argumentValues = Gen.choose(-100, 100)
    val nonZeroArgumentValues = argumentValues.suchThat(_ != 0)
    val constants = Gen.choose(1, 10)

    test("Functions of class 'var % const - const' compute properly with valid inputs") {
        check(Prop.forAll(argumentNames, constants, constants, argumentValues) {
            (arg: String, c1: Int, c2: Int, param: Int) => {
                val func = Function.parse(("(%s : Int) => %s %% %d - %d").format(arg, arg, c1, c2))
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(param % c1 - c2)
            }
        })
    }

    test("Functions of class 'const % var - const' compute properly with valid inputs") {
        check(Prop.forAll(argumentNames, constants, constants, nonZeroArgumentValues) {
            (arg: String, c1: Int, c2: Int, param: Int) => {
                val func = Function.parse(("(%s : Int) => %d %% %s - %d").format(arg, c1, arg, c2))
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(c1 % param - c2)
            }
        })
    }

    test("Functions of class 'const % const - var' compute properly with valid inputs") {
        check(Prop.forAll(argumentNames, constants, constants, argumentValues) {
            (arg: String, c1: Int, c2: Int, param: Int) => {
                val func = Function.parse(("(%s : Int) => %d %% %d - %s").format(arg, c1, c2, arg))
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(c1 % c2 - param)
            }
        })
    }

    test("Functions of class 'var % const - var' compute properly with valid inputs") {
        check(Prop.forAll(argumentNames, constants, argumentValues) {
            (arg: String, c1: Int, param: Int) => {
                val func = Function.parse(("(%s : Int) => %s %% %d - %s").format(arg, arg, c1, arg))
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(param % c1 - param)
            }
        })
    }

    test("Functions of class 'const % var - var' compute properly with valid inputs") {
        check(Prop.forAll(argumentNames, constants, nonZeroArgumentValues) {
            (arg: String, c1: Int, param: Int) => {
                val func = Function.parse(("(%s : Int) => %d %% %s - %s").format(arg, c1, arg, arg))
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(c1 % param - param)
            }
        })
    }

    test("Functions of class 'var - const % var' compute properly with valid inputs") {
        check(Prop.forAll(argumentNames, constants, nonZeroArgumentValues) {
            (arg: String, c1: Int, param: Int) => {
                val func = Function.parse(("(%s : Int) => %s - %d %% %s").format(arg, arg, c1, arg))
                func.flatMap((f: (Int => Option[Int])) => f(param)) == Some(param - c1 % param)
            }
        })
    }
}
