package randomapi

enum Operator:
  case Add
  case Sub
  case Mul
  case Div

  override def toString: String =
    this match
      case Add => "+"
      case Sub => "-"
      case Mul => "*"
      case Div => "/"

  def toFunction: (Int, Int) => Int =
    this match
      case Add => _ + _
      case Sub => _ - _
      case Mul => _ * _
      case Div => _ / _