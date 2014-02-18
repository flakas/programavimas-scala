package languageops

import org.scalatest.FunSuite

class TestFunction extends FunSuite {
    test("regular working modulo and subtraction") {
        expect(5) { Function.parse("(x: Int) => 5 - x % x")(17) }
        expect(-6) { Function.parse("(x: Int) => 1 - x % 10")(17) }
    }

    test("division by zero returns false") {
        expect(false) { Function.parse("(x: Int) => 1 - x % x")(0) }
    }
}
