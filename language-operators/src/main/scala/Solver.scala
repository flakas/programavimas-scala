package languageops
import scala.util.Random
import scala.concurrent.Await
import scala.concurrent.duration._
import dispatch._, Defaults._

class Solver(lang: Int, challengeId: Int, token: String) {
    val finder = new SolutionFinder
    val randomizedArgs = ((0 to 100).zip((-100 to 0).reverse)).map(x => x._1 :: x._2 :: List()).flatten.distinct
    //val challengeHost = "guess.homedir.eu"
    //val address = host(challengeHost) / "lang" / lang / "challenge" / challengeId
    val challengeHost = "localhost"
    val address = host(challengeHost) / "scalaguess" / "index.php"

    def guessAnotherPoint(args: List[(Int, Option[Int])]): List[(Int, Option[Int])] = {
        val newArgument = randomizedArgs(args.length)
        val newValue = getValueByArgument(newArgument)
        val result = (newArgument, newValue) :: args
        println("Guessing new point" + result)
        result
    }

    def getValueByArgument(arg: Int): Option[Int] = {
        val request = address
        val myGet = request.GET <<? Map("arg" -> arg.toString, "token" -> token, "challenge" -> challengeId.toString)
        val future = Http(myGet OK as.String).option
        val result = Await.result(future, 10 seconds)
        result.map(_.toInt)
    }

    def postSolution(solution: Op) = {
        val request = address
        val post = request.POST <<? Map("token" -> token, "challenge" -> challengeId.toString)
        val postWithArgs = post << "(x: Int) => " + solution.toString
        val future = Http(postWithArgs OK as.String).option
        Await.result(future, 10 seconds)
    }

    def solve(): Option[Op] = {
        // Get a new data point by creating a random point not already used and querying the server for the value
        // Filter the solution stream with the new data
        // Continue until the stream contains 1 value
        // Return that value

        def solutionCountIsTwo(solutionCount: Int, solutionCounts: List[Int]): Boolean = {
            val sameSolutionCounts = solutionCounts.takeWhile(_ == solutionCount)
            sameSolutionCounts.length == 1
        }

        def findSolution(args: List[(Int, Option[Int])], solutionCounts: List[Int]): Option[Op] = {
            val possibleSolutions = finder.findSolutions(args)
            println("Attempt #" + args.length)
            if (possibleSolutions.length > 1) {
                println("There still are " + possibleSolutions.length + " solutions")
                if (solutionCountIsTwo(possibleSolutions.length, solutionCounts)) {
                    println("possible solution count did not go down, attempting to guess the function")
                    val solution = possibleSolutions.head
                    println("Challenge solved?")
                    val response = postSolution(solution)
                    println(response)
                    if (!response.isEmpty) {
                        println("Challenge solved!")
                        Some(solution)
                    } else {
                        findSolution(guessAnotherPoint(args), possibleSolutions.length :: solutionCounts)
                    }
                } else {
                    findSolution(guessAnotherPoint(args), possibleSolutions.length :: solutionCounts)
                }
            } else if (possibleSolutions.size == 1) {
                val solution = possibleSolutions.head
                println("Challenge solved?")
                val response = postSolution(solution)
                println(response)
                if (!response.isEmpty) {
                    println("Challenge solved!!")
                } else {
                    println("False positive :(")
                }
                Some(solution)
            } else {
                println("Time to panic, no solution found")
                None
            }
        }

        findSolution(List(), List(finder.findSolutions(List()).length+1))
    }
}
