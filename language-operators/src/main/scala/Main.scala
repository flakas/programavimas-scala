package languageops

object Main extends App {
    val f = Function.parse("(pq: Int) => 1 - pq % pq")
    println(f(0))
}
