package randomapi
import ParametricRandom.*

/* Functional Parametric Random class. */
class ParametricRandom(val parameter: Vector[Byte], val rng: RNG, val idx: Int = 0) extends RNG:
  // Design decision: increment by 1 or double size?
  def nextInt(): (Int, RNG) = 
    val (newParam, newRng) = if idx + SizeOfInt > parameter.size then 
      val (newBytes, newRng) = rng.nextBytes(idx - parameter.size + SizeOfInt)
      (parameter ++ newBytes, newRng)
    else (parameter, rng)

    var myNum = 0
    for (i <- 0 until SizeOfInt)
      myNum = myNum << BitsInByte
      myNum += newParam(idx + i).toInt
    (myNum, ParametricRandom(newParam, newRng, idx + SizeOfInt))
  // Mod gives close enough approximation for reasonably sized n
  def nextInt(n: Int): (Int, RNG) = 
    val (newInt, newRng) = this.nextInt()
    val rangeCorrected = newInt % n
    val signCorrected = if rangeCorrected >= 0 then rangeCorrected else rangeCorrected + n
    (signCorrected, newRng)  
  def nextDouble(): (Double, RNG) = 
    val (newInt, newRng) = this.nextInt(Int.MaxValue)
    (newInt.toFloat / (Int.MaxValue + 1), newRng)
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
