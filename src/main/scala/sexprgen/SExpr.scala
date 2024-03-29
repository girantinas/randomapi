package sexprgen

import randomapi.{Gen, ParametricRandom, RNG, ScalaRandom}

enum SExpr:
  case Expression(operator: Operator, e1: SExpr, e2: SExpr)
  case Num(n: Int)

  def evaluate: Option[Long] = 
    this match
      case Num(n) => Some(n)
      case Expression(operator, e1, e2) => 
        try {
          (for {
            v1 <- e1.evaluate
            v2 <- e2.evaluate
          } yield(operator.toFunction(v1, v2)))
        } catch {
          case e: ArithmeticException => None
        }
    
  def length: Int = // Hopefully should be not too expensive...
    this match
      case Num(n) => n.toString.length()
      case Expression(operator, e1, e2) => 3 + e1.length + e2.length

  
  override def toString(): String =
    this match
      case Num(n) => n.toString()
      case Expression(operator, e1, e2) => ("(" ++ e1.toString ++ " " ++ operator.toString ++ " " ++ e2.toString ++ ")")

object SExpr:
  /* Makes expressions iteratively. */
  def genSExprSimple(operator: Gen[Operator], literal1: Gen[Int], literal2: Gen[Int]): Gen[SExpr] =
    for {
      op <- operator
      lit1 <- literal1
      lit2 <- literal2
    } yield SExpr.Expression(op, SExpr.Num(lit1), SExpr.Num(lit2))
  
  /* Makes expressions recursively. */
  val maxDepth = 10
  val maxNum = 30
  def genSExprRec(depth: Int = 0): Gen[SExpr] =
    for {
      rand <- Gen.double.mark(DecisionType.Structure)
      expr <- if (rand < 1 - depth.toDouble / maxDepth) 
        then for {
          op <- genOperator.mark(DecisionType.Operator)
          exp1 <- genSExprRec(depth + 1)
          exp2 <- genSExprRec(depth + 1)
        } yield (SExpr.Expression(op, exp1, exp2))
        else for {
          n <- Gen.range(0, maxNum).mark(DecisionType.Number)
        } yield (SExpr.Num(n))
    } yield (expr)

  def genOperator: Gen[Operator] = Gen.oneOf(Operator.values)
  
  def generationTrial(rng: RNG): Unit =
    val expression1 = genSExprRec().generate(rng)
    println(expression1)
    println(s"Value: ${expression1.evaluate.getOrElse("Error")}")

    val opGen = Gen.oneOf(Operator.values)
    val lit1 = Gen.range(0, 10)
    val lit2 = Gen.range(11, 20)
    val expression2 = genSExprSimple(opGen, lit1, lit2).generate(rng)
    println(expression2)

    val numExprs = Gen.range(1, 10)
    val expressionsGen = numExprs.flatMap { n => Gen.seqToGen(Seq.fill(n)(genSExprSimple(opGen, lit1, lit2))) }
    val expressions = expressionsGen.generate(rng)
    println(expressions)

  // Define constraints to pass to a constraint solver
  // z3, cvc5, etc - SMT, LP, ILP solvers: choose theory of bit vectors b/c circuits are guaranteed to be piles of bit vectors
  // chisel -> firrtl -> transition system -> smt formula -> smt solver
  // constraint : chisel Bundle -> Bool

  // Biases?

  def main(args: Array[String]): Unit = 
    val is = List(2, 94, 6509, 347)
    
    println("\n\n")
    println("Generation with Parametric Random:")
    println("-"*30)
    is.foreach { i =>
      generationTrial(ParametricRandom.fromSeed(i))
    }
  