package languageops

object Main extends App {
    if (args.length != 2) {
        println("Usage: sbt \"run <function> <argument>\"")
    } else {
        println(args(0))
        val f = Function.parse(args(0))
        f match {
            case Some(func : (Int => Option[Int])) => println(func(args(1).toInt))
            case _ => println("Failed to parse the function")
        }
    }
}
