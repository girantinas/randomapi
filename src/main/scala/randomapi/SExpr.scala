package randomapi

enum SExpr:
  case Expression(operator: Operator, e1: SExpr, e2: SExpr)
  case Num(n: Int)

  def evaluate: Option[Int] = 
    this match
      case Num(n) => Some(n)
      case Expression(operator, e1, e2) => 
        try {
          for 
            v1 <- e1.evaluate
            v2 <- e2.evaluate
          yield(operator.toFunction(v1, v2))
        } catch {
          case e: ArithmeticException => None
        }
  
  override def toString(): String =
    this match
      case Num(n) => n.toString()
      case Expression(operator, e1, e2) => ("(" ++ e1.toString ++ " " ++ operator.toString ++ " " ++ e2.toString ++ ")")