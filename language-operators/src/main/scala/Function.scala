package languageops

abstract class Op {
    def compute(x: Int) : Int
    override def toString : String
}
case class Var(name: String) extends Op {
    def compute(x: Int) = x
    override def toString : String = name
}
case class Constant(value: String) extends Op {
    def compute(x: Int) = value.toInt
    override def toString : String = value
}
case class Modulo(op1: Op, op2: Op) extends Op {
    def compute(x: Int) = op1.compute(x) % op2.compute(x)
    override def toString : String = op1 + " % " + op2
}
case class Subtract(op1: Op, op2: Op) extends Op {
    def compute(x: Int) = op1.compute(x) - op2.compute(x)
    override def toString : String = op1 + " - " + op2
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
                case Array(name, "Int") => Some(name)
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
            } else if (s.indexOf("%") != -1) {
                val components = s.split("%").map(_.trim)
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
        if (functionComponents.length == 2) {
            for {
                variableName <- parseHead(functionComponents(0))
                f <- parseBody(functionComponents(1), variableName)
            } yield {
                (x: Int) => {
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
                }
            }
        } else {
            None
        }
    }
}
