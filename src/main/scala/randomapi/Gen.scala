package randomapi

import java.nio.file.{Paths, Files}
import scala.util.Random
import scala.collection.immutable.Vector
import scala.{Enumeration}
import scala.annotation.tailrec
import reflect.Enum

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
  
  extension [A](self: Gen[A]) def generateWithRNG(rng: RNG): (A, RNG) = self(rng)

  extension [A](self: Gen[A]) def mark[B](label: B): Gen[A] = 
    (rng: RNG) => 
      val (value, rng2) = self(rng)
      rng2 match {
      case rng2: MarkRNG[B] =>
        val rng3 = rng2.correlate(label)
        (value, rng3)
      case _ => { (value, rng2) }
    }

  extension [A](self: Gen[A]) def ensureNot(a: A): Gen[A] =
    (rng: RNG) =>
      val (value, rng2) = self(rng)
      if value != a then (value, rng2) else self.ensureNot(a)(rng2)

  def lift[A](a: A): Gen[A] = (a, _)

  /* Generates an integer uniformly between lower inclusive and upper exclusive.  */
  def range(lower: Int, upper: Int): Gen[Int] = 
    (rng: RNG) => 
      val (offset, rng2) = rng.nextInt(upper - lower) 
      (lower + offset, rng2)

  /* Generates an integer based on a probability distribution between lower inclusive and upper exclusive. 
  The probability distribution need not be normalized.  */
  def biasedRange(lower: Int, upper: Int, biases: Seq[Double]): Gen[Int] =
    assert(upper - lower == biases.length, "Biases are not the correct length!")
    assert(biases.forall(_ >= 0), "Some biases are negative.")
    val totalProb = biases.sum
    val normBiases = biases.map{ b => b / biases.sum }
    for {
      prob <- Gen.double
    } yield ( normBiases.foldLeft((0, prob))((idxp, value) => if idxp(1) <= 0 then idxp else (idxp(0) + 1, idxp(1) - value))(0) - 1 )

  def nonNegativeInt: Gen[Int] = (rng: RNG) =>
    val (i, r) = rng.nextInt()
    (if i < 0 then -(i + 1) else i, r)

  def int: Gen[Int] = _.nextInt()

  /* Generates a double uniformly between 0 and 1. */
  def double: Gen[Double] = (rng: RNG) => rng.nextDouble()

  /* Generates one element of a sequence. Currently uniform, can add bias */
  def oneOf[B](seq: Seq[B], biases: Option[Seq[Double]] = None): Gen[B] =
    if biases == None then
      map(range(0, seq.size)) { idx => seq(idx) }
    else
      map(biasedRange(0, seq.size, biases.get)) { idx => seq(idx) }

  def oneOf[B](arr: Array[B]): Gen[B] = 
    map(range(0, arr.size)) { idx => arr(idx) }

  def oneOf[B <: Enumeration](en: B): Gen[en.Value] = oneOf(en.values.toSeq)

  // Not very satisfying...
  def oneOf[B <: Enum](en: B): Gen[B] = oneOf(en.productIterator.toSeq).map(x => x.asInstanceOf[B])

  // todo: oneOf for typeclasses
  def nBitSignedInt(n: Int): Gen[Int] =
    assert(n > 0, "Not a valid amount of bits.") 
    Gen.range(-Math.pow(2, n - 1).toInt, Math.pow(2, n - 1).toInt)

  def nBitUnsignedInt(n: Int): Gen[Int] =
    assert(n > 0, "Not a valid amount of bits.")
    Gen.range(0, Math.pow(2, n).toInt)

  def seqToGen[B](seq: Seq[Gen[B]]): Gen[Seq[B]] =
    (rng: RNG) => seq.foldRight[(Seq[B], RNG)]((Seq.empty, rng)){(gen, pair) => 
      val (soFar, r) = pair
      val (next, r2) = gen(r)
      (next +: soFar, r2)
    }

  // extension [A](self: Seq[Gen[A]]) def toGen(): Gen[Seq[A]] = seqToGen(self)

  // cats .sequence

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

  /* Custom distros */
  
  def keepTrying[T](gen: Gen[Option[T]]): Gen[T] = ???

  /**
   * Generates Double values according to the given gaussian
   * distribution, specified by its mean and standard deviation.
   */
  def normal(mean: Double, stdDev: Double): Gen[Double] =
    // Box-Muller Algorithm
    // @tailrec TODO: trampolining (tailRecM)
    // def tailRecM[A, B](init: A)(fn: A => Gen[Either[A, B]]): Gen[B] =
    // On left, continue looping and on right life the value
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
    // @tailrec
    def loop(trialsLeft: Int, successes: Int): Gen[Int] =
      if (trialsLeft == 0) then lift(successes)
      else test.map(if _ then 1 else 0).flatMap(i => loop(trialsLeft - 1, successes + i))

    loop(trials, 0)
