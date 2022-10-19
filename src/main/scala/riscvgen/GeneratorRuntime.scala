package riscvgen

object GeneratorRuntime {
  def createInstructionClasses(targetString: String): Unit = 
    println(RiscvInstrGroup.valueOf(targetString.toUpperCase()))

  val usage = """
  Usage: riscvgen --target isa_name --output dir_name
  """
  def main(args: Array[String]): Unit =
    if (args.length == 0) println(usage)

    def nextArg(map: Map[String, String], list: List[String]): Map[String, String] = {
      list match {
        case Nil => map
        case "--target" :: value :: tail =>
          nextArg(map ++ Map("target" -> value), tail)
        case "--output" :: value :: tail =>
          nextArg(map ++ Map("output" -> value), tail)
        case unknown :: _ =>
          println("Unknown option " + unknown)
          System.exit(1)
          Map()
      }
    }
    val options = nextArg(Map("target" -> "rv32i", "output" -> "./out"), args.toList)

    createInstructionClasses(options("target"))
}