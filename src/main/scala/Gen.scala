package randomapi

import java.nio.file.{Paths, Files}
import scala.util.Random
import scala.collection.immutable.Vector
import scala.{Enumeration}
import scala.annotation.tailrec

opaque type State[S, +A] = S => (A, S)
opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A]) def flatMap[B](f: A => Gen[B]): Gen[B] = 
    (rng: RNG) => 
      val (value, rng2) = self(rng)
      f(value)(rng2)

  extension [A](self: Gen[A]) def map[B](f: A => B): Gen[B] = 
    (rng: RNG) =>
      val (value, rng2) = self(rng)
      (f(value), rng2)

  extension [A](self: Gen[A]) def generate(rng: RNG): A = self(rng)(0)

  def lift[A](a: A): Gen[A] = (a, _)

  /* Generates an integer uniformly between lower inclusive and upper exclusive.  */
  def range(lower: Int, upper: Int): Gen[Int] = 
    (rng: RNG) => 
      val (offset, rng2) = rng.nextInt(upper - lower + 1) 
      (lower + offset, rng2)

  /* Generates a double uniformly between 0 and 1. */
  def double: Gen[Double] = (rng: RNG) => rng.nextDouble()

  /* Generates one element of a sequence. Currently uniform, can add bias */
  def oneOf[B](seq: Seq[B]): Gen[B] =
    map(range(0, seq.size)) { idx => seq(idx) }

  def oneOf[B](arr: Array[B]): Gen[B] = 
    map(range(0, arr.size)) { idx => arr(idx) }

  def oneOf[B <: Enumeration](en: B): Gen[en.Value] = oneOf(en.values.toSeq)

  def seqToGen[B](seq: Seq[Gen[B]]): Gen[Seq[B]] =
    (rng: RNG) => seq.foldRight[(Seq[B], RNG)]((Seq.empty, rng)){(gen, pair) => 
      val (soFar, r) = pair
      val (next, r2) = gen(r)
      (next +: soFar, r2)
    }

  // cats .sequence

  /* Makes expressions iteratively. */
  def genSExprSimple(operator: Gen[Operator], literal1: Gen[Int], literal2: Gen[Int]): Gen[SExpr] =
    for {
      op <- operator
      lit1 <- literal1
      lit2 <- literal2
    } yield SExpr.Expression(op, SExpr.Num(lit1), SExpr.Num(lit2))
  
  /* Makes expressions recursively. */
  val max_depth = 10
  val max_num = 20
  def genSExprRec(depth: Int = 0): Gen[SExpr] =
    double.flatMap { (rand: Double) =>
      if (rand < 1 - depth.toDouble / max_depth) 
      then for {
        op <- genOperator
        exp1 <- genSExprRec(depth + 1)
        exp2 <- genSExprRec(depth + 1)
      } yield (SExpr.Expression(op, exp1, exp2))
      else for {
        n <- range(0, max_num)
      } yield (SExpr.Num(n))
    }

  def genOperator: Gen[Operator] = Gen.oneOf(Operator.values)

    /**
   * Generates a Boolean which has the given chance to be true.
   *
   *  - bernoulli(1.0) is always true
   *  - bernoulli(0.5) is true 50% of the time
   *  - bernoulli(0.1) is true 10% of the time
   *  - bernoulli(0.0) is never true
   */
  def bernoulli(prob: Double): Gen[Boolean] =
    // Negatives are fine
    for {
      x <- double
    } yield(x < prob)

  /**
   * Generates Double values according to the given gaussian
   * distribution, specified by its mean and standard deviation.
   */
  def normal(mean: Double, stdDev: Double): Gen[Double] =
    // Box-Muller Algorithm
    // @tailrec TODO
    def loop(x: Double, y: Double): Gen[Double] =
      val s = x * x + y * y
      if (s < 1.0 && s > 0.0) then {
        val scale = stdDev * Math.sqrt(-2.0 * Math.log(s) / s)
        lift(x * scale + mean)
      } else for {
        x <- double
        y <- double
        z <- loop(x, y)
      } yield z

    loop(0, 0)

  /**
   * Generates Double values according to the given exponential
   * distribution, specified by its rate parameter.
   */
  def exponential(rate: Double): Gen[Double] = {
    require(rate > 0.0, s"Rate must be positive")
    for {
      x <- double
    } yield (- Math.log(x) / rate)
  }

  /**
   * Generates Int values according to the given geometric
   * distribution, specified by its success probability.
   */
  def geometric(prob: Double): Gen[Int] =
    require(prob > 0.0, s"prob must be positive")
    for { 
      u <- double
    } yield (Math.log(u) / Math.log1p(-prob)).toInt

  /**
   * Generates Int values according to the given Poisson distribution,
   * specified by its rate parameter.
   */
  // def poisson(rate: Double): Gen[Int] =
  //   require(rate > 0, s"Rate must be positive.")
  //   ???

  /**
   * Generates Int values according to the given binomial
   * distribution, specified by the number of trials to conduct, and
   * the generator of a true test.
   */
  def binomial(test: Gen[Boolean], trials: Int): Gen[Int] =
    // @tailrec TODO
    def loop(trialsLeft: Int, successes: Int): Gen[Int] =
      if (trialsLeft == 0) then lift(successes)
      else test.map(if _ then 1 else 0).flatMap(i => loop(trialsLeft - 1, successes + i))

    loop(trials, 0)

def generationTrial(rng: RNG): Unit =
  val expression1 = Gen.genSExprRec().generate(rng)
  println(expression1)
  println(s"Value: ${expression1.evaluate.getOrElse("Error")}")

  val opGen = Gen.oneOf(Operator.values)
  val lit1 = Gen.range(0, 10)
  val lit2 = Gen.range(11, 20)
  val expression2 = Gen.genSExprSimple(opGen, lit1, lit2).generate(rng)
  println(expression2)

  val numExprs = Gen.range(1, 10)
  val expressionsGen = numExprs.flatMap { n => Gen.seqToGen(Seq.fill(n)(Gen.genSExprSimple(opGen, lit1, lit2))) }
  val expressions = expressionsGen.generate(rng)
  println(expressions)

// Define constraints to pass to a constraint solver
// z3, cvc5, etc - SMT, LP, ILP solvers: choose theory of bit vectors b/c circuits are guaranteed to be piles of bit vectors
// chisel -> firrtl -> transition system -> smt formula -> smt solver
// constraint : chisel Bundle -> Bool

// Biases?

def main(args: Array[String]): Unit = 
  val is = List(2, 94, 6509, 347)
  println("Generation with Scala Random:")
  println("-"*30)
  is.foreach { i =>
    generationTrial(ScalaRandom(i))
  }
  println("\n\n")
  println("Generation with Parametric Random:")
  println("-"*30)
  is.foreach { i =>
    generationTrial(ParametricRandom.fromSeed(i))
  }