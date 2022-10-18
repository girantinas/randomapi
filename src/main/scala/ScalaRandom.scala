package randomapi

import scala.util.Random

/* Fake functional RNG Class to query the Scala random. How to fix? Perhaps copy the generator every time? */
/* Maintain state manually. */
class ScalaRandom(val seed: Long) extends RNG:
  val random = Random(seed)
  def nextInt(n: Int): (Int, RNG) = (random.nextInt(n), this)
  def nextInt(): (Int, RNG) = (random.nextInt(), this)
  def nextDouble(): (Double, RNG) = (random.nextDouble(), this)
  def nextBool(): (Boolean, RNG) = (random.nextBoolean(), this)
  def nextBytes(n: Int): (Vector[Byte], RNG) = (random.nextBytes(n).toVector, this)
  def reset(): RNG = ScalaRandom(seed)
