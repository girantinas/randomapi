package randomapi

import ParametricRandom.*
import scala.annotation.tailrec

/* Functional Parametric Random class. */
class ParametricRandom(val parameter: Vector[Byte], val rng: RNG, val idx: Int = 0) extends RNG:
  def nextInt(): (Int, RNG) = 
    val (newParam, newRng) = if idx + SizeOfInt > parameter.size then 
      // Double parameter size for amortization
      val (newBytes, newRng) = rng.nextBytes(parameter.size)
      (parameter ++ newBytes, newRng)
    else (parameter, rng)

    var myNum = 0
    for (i <- 0 until SizeOfInt)
      myNum = myNum << BitsInByte
      myNum += newParam(idx + i).toInt
    (myNum, ParametricRandom(newParam, newRng, idx + SizeOfInt))
  @tailrec
  final def nextInt(n: Int): (Int, RNG) = 
    val (newInt, newRng) = this.nextInt()
    if (newInt <  Int.MaxValue - Int.MaxValue % n) then
      val rangeCorrected = newInt % n
      val signCorrected = if rangeCorrected >= 0 then rangeCorrected else rangeCorrected + n
      (signCorrected, newRng)
    else 
      nextInt(n)

  def nextNonNegativeInt(): (Int, RNG) =
    val (myInt, rng2) = this.nextInt()
    (if (myInt < 0) then - (myInt + 1) else myInt, rng2)

  def nextDouble(): (Double, RNG) = 
    val (newInt, newRng) = this.nextNonNegativeInt()
    (newInt / (Int.MaxValue.toDouble + 1), newRng)
  def nextBool(): (Boolean, RNG) = 
    val (newParam, newRng) = if idx + SizeOfBool > parameter.size then 
      val (newBytes, newRng) = rng.nextBytes(idx - parameter.size + SizeOfBool)
      (parameter ++ newBytes, newRng)
    else (parameter, rng)
    (newParam(idx).toInt % 2 == 0, ParametricRandom(newParam, newRng, idx + SizeOfBool))
  def nextBytes(n: Int): (Vector[Byte], RNG) =
    val (newParam, newRng) = if idx + n > parameter.size then 
      val (newBytes, newRng) = rng.nextBytes(idx - parameter.size + n)
      (parameter ++ newBytes, newRng)
    else (parameter, rng)
    (newParam.slice(idx, idx + n), ParametricRandom(newParam, newRng, idx + n))
  def reset(): RNG = ParametricRandom(parameter, rng)

  // Mutates n bits (with replacement) of the parameter using the underlying non-parametric rng
  // If you want to mutate a lot (more than O(sqrt(n)) things) then use without replacement
  def mutate(n: Int): ParametricRandom =
    assert(n > 0, "The number of mutated bits must be positive.")
    assert(n <= parameter.length * 8, "The number of mutated bits must be less than the number of parameters.")
    val (newRNG, places) = Seq.fill(n)(0).foldLeft((rng, List[Int]())){ 
      (x, _) => 
        val (r, l) = x
        val (place, retR) = r.nextInt(parameter.length * 8)
        (retR, place :: l)
    }
    val newParam = places.foldLeft(parameter){ (par, place) =>
      val byteNum = place / 8
      val bitNum = place % 8
      par.slice(0, byteNum) ++ Vector((par(byteNum) ^ (0x1 << bitNum)).toByte) ++ par.slice(byteNum + 1, par.length)
    }

    ParametricRandom(newParam, newRNG)

object ParametricRandom:
  val SizeOfInt = 4
  val SizeOfBool = 1 // We take the whole byte for a Bool
  val SizeOfChar = 1
  val BitsInByte = 8
  val InitialNumParameter = 20

  def fromSeed(startSeed: Long): ParametricRandom =
    val rand = new ScalaRandom(startSeed)
    val (bytes, rng) = rand.nextBytes(InitialNumParameter)
    ParametricRandom(bytes, rng)
