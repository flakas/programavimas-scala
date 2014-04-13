package languageops

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Arbitrary._

class SolverSpec extends FlatSpec with Checkers with Matchers {

    class Argument(x: Int) {
        def get = x
        override def toString = x.toString
    }

    class Sampler(f: Op, args: List[Argument]) {
        def getSamples = args.map(x => {
            val xValue = x.get
            try {
                (xValue, Some(f.compute(xValue)))
            } catch {
                case e: ArithmeticException => (xValue, None)
            }
        })
        override def toString = "Sampler(" + f + " " + getSamples + ")"
    }

    implicit lazy val nonZeroArgumentGenerator: Arbitrary[Argument] = Arbitrary {
        for {
            c <- Gen.choose(-100, 100)
        } yield new Argument(c)
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

    class ConcreteChallengeVerifier(function: Op) extends ChallengeVerifier {
        val solutionString = "(x: Int) => " + function.toString
        def sample(arg: Int): Option[Int] = {
            try {
                Some(function.compute(arg))
            } catch {
                case e: ArithmeticException => None
            }
        }

        def guessSolution(solution: String): Option[String] = {
            if (solution == solutionString) {
                Some("OK")
            } else {
                None
            }
        }
    }

    "Solver" should "always find the solution" in {
        check(Prop.forAll {
            (function: Op) => {
                val verifier = new ConcreteChallengeVerifier(function)
                val solver = new Solver(verifier)
                solver.solve.get == function
            }
        })
    }
}

