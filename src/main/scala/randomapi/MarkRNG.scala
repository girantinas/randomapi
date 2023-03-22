package randomapi

enum ParameterState[A]:
  case Unused()
  case Unmarked()
  case Marked(a: A)

trait MarkRNG[A](val parameter: Vector[Byte], val correlationMap: Vector[ParameterState[A]]) extends RNG:
  // Correlates all unmarked random bytes with a certain label `a`
  def correlate(a: A): MarkRNG[A]