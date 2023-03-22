package randomapi

import ParametricRandom.*
import ParameterState.*
import scala.annotation.tailrec

/* Functional Parametric Random class. */
class ParametricRandom[A](private val par: Vector[Byte], private val corr: Vector[ParameterState[A]], val rng: RNG, val idx: Int = 0) extends MarkRNG[A](par, corr):
  def nextInt(): (Int, RNG) = 
    val (newParam, newCorr, newRng) = if idx + SizeOfInt > parameter.size then 
      // Double parameter size for amortization
      val (newBytes, newRng) = rng.nextBytes(parameter.size)
      (parameter ++ newBytes, corr ++ Vector.fill(parameter.size)(Unused()), newRng)
    else (parameter, corr, rng)

    var myNum = 0
    for (i <- 0 until SizeOfInt)
      myNum = myNum << BitsInByte
      myNum += newParam(idx + i).toInt
    val newCorr2 = newCorr.slice(0, idx) ++ Vector.fill(SizeOfInt)(Unmarked()) ++ newCorr.slice(idx + SizeOfInt, newCorr.size)
    (myNum, ParametricRandom(newParam, newCorr2, newRng, idx + SizeOfInt))
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
    val (newParam, newCorr, newRng) = if idx + SizeOfBool > parameter.size then 
      val (newBytes, newRng) = rng.nextBytes(idx - parameter.size + SizeOfBool)
      (parameter ++ newBytes, corr ++ Vector.fill(idx - parameter.size + SizeOfBool)(Unused()), newRng)
    else (parameter, corr, rng)
    (newParam(idx).toInt % 2 == 0, ParametricRandom(newParam, newCorr.updated(idx, Unmarked()), newRng, idx + SizeOfBool))
  def nextBytes(n: Int): (Vector[Byte], RNG) =
    val (newParam, newCorr, newRng) = if idx + n > parameter.size then 
      val (newBytes, newRng) = rng.nextBytes(idx - parameter.size + n)
      (parameter ++ newBytes, corr ++ Vector.fill(idx - parameter.size + n)(Unused()), newRng)
    else (parameter, corr, rng)
    (newParam.slice(idx, idx + n), ParametricRandom(newParam, newCorr.slice(0, idx) ++ Vector.fill(n)(Unmarked()) ++ newCorr.slice(idx + n, newCorr.size), newRng, idx + n))
  def reset(): RNG = ParametricRandom(parameter, Vector.fill(parameter.length)(Unused()), rng.reset())

  // Mutates n bits (with replacement) of the parameter using an external RNG
  def mutate(n: Int, externalRng: RNG): ParametricRandom[A] =
    assert(n > 0, "The number of mutated bits must be positive.")
    assert(n <= parameter.length * 8, "The number of mutated bits must be less than the number of parameters.")
    val (newRNG, places) = Seq.fill(n)(0).foldLeft((externalRng, List[Int]())){ 
      (x, _) => 
        val (r, l) = x
        val (place, retR) = r.nextInt(parameter.length * 8)
        (retR, place :: l)
    }
    val newParam = places.foldLeft(parameter){ (par, place) =>
      val byteNum = place / BitsInByte
      val bitNum = place % BitsInByte
      par.slice(0, byteNum) ++ Vector((par(byteNum) ^ (0x1 << bitNum)).toByte) ++ par.slice(byteNum + 1, par.length)
    }
    ParametricRandom(newParam, corr, rng)

  // Mutates bits of the parameter according to their marking. Empties the correlation map
  def mutate(markingProb: A => Double, externalRng: RNG): ParametricRandom[A] =
    val newParam = corr.zipWithIndex
        .filter((state, i) => state match {
          case Marked(a) => true
          case _ => false
        }).foldLeft((externalRng, List[(Int, Byte)]()))((pair1, pair2) => 
          val (currRng, currList) = pair1
          val (state: Marked[A], i) = pair2: @unchecked
          val prob = markingProb(state.a)
          var (sampleProb, newRNG) = currRng.nextDouble()
          if (sampleProb > prob) then {
            val currByte = parameter(i)
            val (bitNum, newRNG2) = newRNG.nextInt(BitsInByte) // Choose a bit randomly to flip
            (newRNG2, currList :+ (i, (currByte ^ (0x1 << bitNum)).toByte))
          } else (newRNG, currList)
        )(1).foldLeft(par)((param, pair) => param.updated(pair(0), pair(1)))
    ParametricRandom(newParam, Vector.fill(newParam.length)(Unused()), rng)

  def correlate(a: A): ParametricRandom[A] =
    // TODO: fix bug
    ParametricRandom(parameter, corr.map(
      (o: ParameterState[A]) =>
      o match {
      case Unmarked() => Marked(a)
      case x => x
    }), rng, idx)

  def decorrelate(): ParametricRandom[A] =
    ParametricRandom(parameter, Vector.fill(parameter.length)(Unused()), rng)

object ParametricRandom:
  val SizeOfInt = 4
  val SizeOfBool = 1 // We take the whole byte for a Bool
  val SizeOfChar = 1
  val BitsInByte = 8
  val InitialNumParameter = 20

  def fromSeed[A](startSeed: Long): ParametricRandom[A] =
    val rand = new PseudoRandom(startSeed)
    val (bytes, rng) = rand.nextBytes(InitialNumParameter)
    val correlationMap = Vector.fill(InitialNumParameter)(Unused[A]())
    ParametricRandom(bytes, correlationMap, rng)
