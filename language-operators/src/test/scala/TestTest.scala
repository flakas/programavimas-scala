package languageops

import org.scalatest.FunSuite

class TestFunction extends FunSuite {
    test("correctly calculates function output") {
        expect(Some(5)) {
            Function.parse("(x: Int) => 5 - x % x") match {
                case Some(f: (Int => Some[Int])) => f(17)
                case _ => None
            }
        }
        expect(Some(-6)) {
            Function.parse("(x: Int) => 1 - x % 10") match {
                case Some(f: (Int => Some[Int])) => f(17)
                case _ => None
            }
        }
    }

    test("division by zero returns false") {
        expect(None) {
            Function.parse("(x: Int) => 1 - x % x") match {
                case Some(f: (Int => Some[Int])) => f(0)
                case _ => None
            }
        }
    }

    test("negative integer overflows are handled properly") {
        expect(Some(2147483549)) {
            Function.parse("(x: Int) => x % 65535 - 2147483647") match {
                case Some(f: (Int => Some[Int])) => f(-100)
                case _ => None
            }
        }
    }

    test("malformed function (missing `=>`) fails") {
        expect(None) { Function.parse("(x: Int) = 1 - x % x") }
    }

    test("does not accept arguments from (-inf, -100) U (100, inf)") {
        expect(None) {
            Function.parse("(x: Int) => 1 - x % 10") match {
                case Some(f: (Int => Some[Int])) => f(-1000)
                case _ => None
            }
        }
        expect(None) {
            Function.parse("(x: Int) => 1 - x % 10") match {
                case Some(f: (Int => Some[Int])) => f(1000)
                case _ => None
            }
        }
    }

    test("verifies that no other variable names are used") {
        expect(None) { Function.parse("(x: Int) => x - y % 10") }
    }
}
