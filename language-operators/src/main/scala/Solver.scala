package languageops

class Solver(verifier: ChallengeVerifier) {
    val finder = new SolutionFinder
    val argsToGuess = ((0 to 100).zip((-100 to 0).reverse)).map(x => x._1 :: x._2 :: List()).flatten.distinct

    def guessAnotherPoint(args: List[(Int, Option[Int])]): List[(Int, Option[Int])] = {
        val newArgument = argsToGuess(args.length)
        val newValue = verifier.sample(newArgument)
        val result = (newArgument, newValue) :: args
        result
    }

    def guessSolution(solution: Op) = {
        verifier.guessSolution("(x: Int) => " + solution.toString)
    }

    def isSearchSpaceExhausted(args: List[(Int, Option[Int])]) =
        args.length >= argsToGuess.length

    def timesSolutionCountHasntChanged(solutionCount: Int, solutionCounts: List[Int]) =
        (solutionCount :: solutionCounts).takeWhile(_ == solutionCount).length

    def solve: Option[Op] = {
        def trySolution(solution: Op) = {
            val response = guessSolution(solution)
            if (response.isEmpty) {
                None
            } else {
                Some(solution)
            }
        }

        def tryAllSolutions(solutions: Stream[Op]): Option[Op] = {
            if (solutions.isEmpty) {
                None
            } else {
                val response = trySolution(solutions.head)
                if (response.isEmpty) {
                    tryAllSolutions(solutions.tail)
                } else {
                    response
                }
            }
        }

        def findSolution(args: List[(Int, Option[Int])], solutionCounts: List[Int]): Option[Op] = {
            val searchSpaceLeft = finder.findSolutions(args)
            val amountOfPossibilities = searchSpaceLeft.length
            if (timesSolutionCountHasntChanged(amountOfPossibilities, solutionCounts) == 2) {
                // Try to catch the correct class of functions early
                val result = trySolution(searchSpaceLeft.head)
                if (result.isEmpty) {
                    findSolution(guessAnotherPoint(args), amountOfPossibilities :: solutionCounts)
                } else {
                    result
                }
            } else if (timesSolutionCountHasntChanged(amountOfPossibilities, solutionCounts) > 10) {
                // Attempt to prevent going through the rest of valid arguments
                // as that may not narrow down the search space
                tryAllSolutions(searchSpaceLeft)
            } else {
                findSolution(guessAnotherPoint(args), amountOfPossibilities :: solutionCounts)
            }
        }

        findSolution(List(), List(finder.findSolutions(List()).length+1))
    }
}
