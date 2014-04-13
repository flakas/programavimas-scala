package languageops

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.Arbitrary._

class SolutionFinderSpec extends FlatSpec with Checkers with Matchers {

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

    implicit lazy val functionsWithSampledValues: Arbitrary[(Op, Sampler)] = Arbitrary {
        for {
            f <- arbitrary[Op]
            arguments <- arbitrary[List[Argument]]
            args = arguments
        } yield (f, new Sampler(f, args))
    }

    "Solution finder" should "generate all valid types of constants" in {
        check(Prop.forAll {
            (constant: Constant) => {
                (new SolutionFinder).constants.contains(constant)
            }
        })
    }

    it should "generate all valid functions" in {
        check(Prop.forAll {
            (function: Op) => {
                (new SolutionFinder).functions.contains(function)
            }
        })
    }

    it should "always have the target function as a result" in {
        check(Prop.forAll {
            (params: (Op, Sampler)) => {
                val finder = new SolutionFinder
                val f = params._1
                val samples = params._2.getSamples
                finder.findSolutions(samples).contains(f)
            }
        })
    }
}
