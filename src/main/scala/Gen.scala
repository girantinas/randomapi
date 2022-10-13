package randomapi

import java.nio.file.{Paths, Files}
import scala.util.Random
import scala.collection.immutable.Vector
import scala.{Enumeration}

opaque type State[S, +A] = S => (A, S)
opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A]) def lift(a: A): Gen[A] = (a, _)

  extension [A](self: Gen[A]) def flatMap[B](f: A => Gen[B]): Gen[B] = 
    (rng: RNG) => 
      val (value, rng2) = self(rng)
      f(value)(rng2)

  extension [A](self: Gen[A]) def map[B](f: A => B): Gen[B] = 
    (rng: RNG) =>
      val (value, rng2) = self(rng)
      (f(value), rng2)

  extension [A](self: Gen[A]) def generate(rng: RNG): A = self(rng)(0)

  /* Generates an integer uniformly between lower and upper exclusive.  */
  def range(lower: Int, upper: Int): Gen[Int] = 
    (rng: RNG) => 
      val (offset, rng2) = rng.nextInt(upper - lower + 1) 
      (lower + offset, rng2)

  /* Generates a float uniformly between 0 and 1. */
  def float: Gen[Float] = (rng: RNG) => rng.nextFloat()

  /* Generates one element of a sequence. Currently uniform, can add bias */
  def oneOf[B](seq: Seq[B]): Gen[B] =
    (rng: RNG) => 
      val (idx, r) = rng.nextInt(seq.size)
      (seq(idx), r)

  def oneOf[B](arr: Array[B]): Gen[B] = 
    (rng: RNG) => 
      val (idx, r) = rng.nextInt(arr.size)
      (arr(idx), r)

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
    float.flatMap { (rand: Float) =>
      if (rand < 1 - depth.toFloat / max_depth) 
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