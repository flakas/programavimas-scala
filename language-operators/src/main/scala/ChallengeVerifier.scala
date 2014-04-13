package languageops

trait ChallengeVerifier {
    def sample(arg: Int): Option[Int]
    def guessSolution(solution: String): Option[String]
}
