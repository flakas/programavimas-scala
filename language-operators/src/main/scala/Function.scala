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
        def parseHead(s: String) = {
            val argument = s.slice(s.indexOf("(")+1, s.indexOf(")"))
            val components = argument.split(":").map(_.trim)
            components match {
                case Array(name, "Int") => name
                case _ => None
            }
        }
        def parseBody(s: String, variableName: String) : Option[Op] = {
            if (s.indexOf("-") != -1) {
                val components = s.split("-").map(_.trim)
                components.map(parseBody(_, variableName)) match {
                    case Array(Some(value1), Some(value2)) => Some(Subtract(value1, value2))
                    case _ => None
                }
                //Subtract(parseBody(components(0)), parseBody(components(1)))
            } else if (s.indexOf("%") != -1) {
                val components = s.split("%").map(_.trim)
                //val v1 = parseBody(components(0))
                //val v2 = parseBody(components(1))
                components.map(parseBody(_, variableName)) match {
                    case Array(Some(value1), Some(value2)) => Some(Modulo(value1, value2))
                    case _ => None
                }
            } else if (isInt(s) == true) {
                Some(Constant(s))
            } else {
                if (s == variableName) {
                    Some(Var(s))
                } else {
                    None
                }
            }
        }
        val functionComponents = s.split("=>").map(_.trim)
        if (functionComponents.length != 2) {
            None
        } else {
            val functionArgumentName = parseHead(functionComponents(0))
            functionArgumentName match {
                case variableName: String => {
                    val emulatedFunction = parseBody(functionComponents(1), variableName)
                    emulatedFunction match {
                        case Some(f) => {
                            Some((x : Int) => {
                                if (-100 <= x && x <= 100) {
                                    // Limit acceptable arguments to interval [-100, 100]
                                    try {
                                        Some(f.compute(x))
                                    } catch {
                                        case e: ArithmeticException => None
                                    }
                                } else {
                                    None
                                }
                            })
                        }
                        case _ => None
                    }
                }
                case _ => None
            }
        }
    }
}
