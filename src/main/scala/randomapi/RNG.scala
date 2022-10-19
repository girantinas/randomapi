package randomapi

trait RNG:
  /* Uniform between [0, n). */
  def nextInt(n: Int): (Int, RNG)
  /* Uniform between (Int.MinValue, Int.MaxValue) */
  def nextInt(): (Int, RNG)
  /* Uniform between [0.0, 1.0] */
  def nextDouble(): (Double, RNG)
  def nextBool(): (Boolean, RNG)
  def nextBytes(n: Int): (Vector[Byte], RNG)
  /* Reset the RNG to create another set of outputs.
  * Creating the same set of outputs in the same order will 
  * be the exact same after the reset. */
  def reset(): RNG

