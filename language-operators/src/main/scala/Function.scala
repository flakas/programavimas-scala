package languageops

abstract class Op {
    def compute(x: Int) : Int
}
case class Var(name: String) extends Op {
    def compute(x: Int) = x
}
case class Constant(value: String) extends Op {
    def compute(x: Int) = value.toInt
}
case class Modulo(op1: Op, op2: Op) extends Op {
    def compute(x: Int) = op1.compute(x) % op2.compute(x)
}
case class Subtract(op1: Op, op2: Op) extends Op {
    def compute(x: Int) = op1.compute(x) - op2.compute(x)
}

object Function {
    private def isInt(s: String) : Boolean = {
        try {
            s.toInt.isInstanceOf[Int]
        } catch {
            case e: Exception => false
        }
    }
    def parse(s: String) = {
        def parseBody(s: String) : Op = {
            if (s.indexOf("-") != -1) {
                val components = s.split("-").map(_.trim)
                Subtract(parseBody(components(0)), parseBody(components(1)))
            } else if (s.indexOf("%") != -1) {
                val components = s.split("%").map(_.trim)
                Modulo(parseBody(components(0)), parseBody(components(1)))
            } else if (isInt(s) == true) {
                Constant(s)
            } else {
                Var(s)
            }
        }
        val functionComponents = s.split("=>").map(_.trim)
        if (functionComponents.length != 2) {
            None
        } else {
            val emulatedFunction = parseBody(functionComponents(1))
            Some((x : Int) => {
                if (-100 <= x && x <= 100) {
                    // Limit acceptable arguments to interval [-100, 100]
                    try {
                        Some(emulatedFunction.compute(x))
                    } catch {
                        case e: ArithmeticException => None
                    }
                } else {
                    None
                }
            })
        }
    }
}
