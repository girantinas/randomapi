package randomapi

/* Truly Functional LCG RNG */
class PseudoRandom(val seed: Long) extends RNG:
  private def next(bits: Int): (Int, PseudoRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    ((newSeed >>> (48 - bits)).toInt, PseudoRandom(newSeed))
  }
  def nextInt(n: Int): (Int, PseudoRandom) = {
    val (newInt, newRng) = this.nextInt()
    if (newInt <  Int.MaxValue - Int.MaxValue % n) then
      val rangeCorrected = newInt % n
      val signCorrected = if rangeCorrected >= 0 then rangeCorrected else rangeCorrected + n
      (signCorrected, newRng)
    else 
      nextInt(n)
  }
  def nextInt(): (Int, PseudoRandom) = next(32)
  def nextDouble(): (Double, PseudoRandom) = { 
    val (msb, rng) = this.next(26)
    val (lsb, rng2) = rng.next(27)
    (((msb << 27).toLong + lsb).toDouble / (1L << 53), rng2)
  }
  def nextBool(): (Boolean, PseudoRandom) = {
    val (bool, rng) = next(1)
    (bool != 0, rng)
  }
  def nextBytes(n: Int): (Vector[Byte], PseudoRandom) = {
    val bytes: Array[Byte] = Array.fill(n)(0)
    var rng: PseudoRandom = this
    var value: Int = 0
    for (i <- Range(0, n, 4)) {
      val result = rng.next(32)
      value = result(0)
      rng = result(1)
      var j = 0
      while (i + j < n && j < 4) {
        bytes(i + j) = (value & 0xff).toByte
        value = value >> 8
        j += 1
      }
    }
    (bytes.toVector, rng)
}
  def reset(): PseudoRandom = PseudoRandom(seed)
