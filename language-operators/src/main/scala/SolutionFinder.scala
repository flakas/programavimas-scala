package languageops

class SolutionFinder {
    lazy val operationOrders : Stream[Boolean] = true #:: false #:: Stream.empty
    lazy val operandPlaceholders: Stream[List[Boolean]] =
        for {
            op1 <- operationOrders
            op2 <- operationOrders
            op3 <- operationOrders
            ops = op1 :: op2 :: op3 :: List()
            if ops.contains(true) && ops.contains(false)
        } yield ops
    lazy val constants: Stream[Constant] =
        for {
            i <- (1 to 10).toStream
        } yield Constant(i.toString)
    lazy val constantThrees: Stream[List[Constant]] =
        for {
            c1 <- constants
            c2 <- constants
            c3 <- constants
        } yield c1 :: c2 :: c3 :: List.empty
    lazy val operands: Stream[List[Op]] =
        (
            for {
                ops <- operandPlaceholders
                consts <- constantThrees
                preparedOperands = ops.zip(consts).map(x => if (x._1) Var("x") else x._2)
            } yield preparedOperands
        ).distinct
    lazy val functions: Stream[Op] = {
        for {
            operationOrder <- operationOrders
            ops <- operands
        } yield {
            if (operationOrder)
                Subtract(Modulo(ops(0), ops(1)), ops(2))
            else
                Subtract(ops(0), Modulo(ops(1), ops(2)))
        }
    }
    def findSolutions(knownValues: List[(Int, Int)]): Stream[Op] =
        functions.filter(f => knownValues.forall(v => f.compute(v._1) == v._2))
}
