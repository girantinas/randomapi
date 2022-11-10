package riscvgen
import randomapi.Gen
import randomapi.RNG
import randomapi.ParametricRandom

enum InstructionSequenceType:
  case PrimitiveInstruction 
  case ReadAfterWrite
  case ForLoop
  case WhileLoop
  case BranchSequence

enum InstructionSequence:
   // PrimitiveInstruction: Wraps some Instruction type
  case PrimitiveInstruction(operator: RiscvOperator)
  case ReadAfterWrite
  case ForLoop(body: InstructionSequence, count: Int)
  case WhileLoop(body: InstructionSequence, cond: RiscvBranch)
  // For now: assume everything is a basic block.
  case BranchSequence(basicBlock1: InstructionSequence, basicBlock2: InstructionSequence, cond: RiscvBranch)

object InstructionSequence:
  // Could be made easier with reflection?
  val MaxDepth = 10
  val InstructionSequenceBiases = {(depth: Int) => 
    val ProbPrimitive = depth.toDouble / MaxDepth
    val ProbNonPrimitive = (1 - depth.toDouble / MaxDepth) / (InstructionSequenceType.values.length - 1)
    Seq(ProbPrimitive) ++ Seq.fill(InstructionSequenceType.values.length - 1)(ProbNonPrimitive)}

  def gen(depth: Int = 0): Gen[InstructionSequence] = 
    for {
      sequenceType <- Gen.oneOf(InstructionSequenceType.values, biases = Some(InstructionSequenceBiases(depth)))
      sequence <- {sequenceType match
        case InstructionSequenceType.ReadAfterWrite => Gen.lift(ReadAfterWrite)

        case InstructionSequenceType.ForLoop => for {
          count <- Gen.range(0, 100)
          body <- gen(depth + 1)
        } yield (ForLoop(body, count))

        case InstructionSequenceType.WhileLoop => for {
          body <- gen(depth + 1)
          cond <- Gen.oneOf(RiscvBranch.values)
        } yield (WhileLoop(body, cond))

        case InstructionSequenceType.PrimitiveInstruction => for {
          op <- Gen.oneOf(RiscvOperator.values)
        } yield(PrimitiveInstruction(op))

        case InstructionSequenceType.BranchSequence => for {
          basicBlock1 <- gen(depth + 1)
          basicBlock2 <- gen(depth + 1)
          cond <- Gen.oneOf(RiscvBranch.values)
        } yield(BranchSequence(basicBlock1, basicBlock2, cond)) }
    } yield (sequence)

  def genInstrsForSeq(instrseq: InstructionSequence, allowedRegs: Seq[RiscvReg] = RiscvReg.values.toIndexedSeq): Gen[Seq[String]] =
    if (allowedRegs.length == 1) then Gen.lift(Seq.empty) else
    instrseq match  
      case ReadAfterWrite =>
        for {
          dest <- Gen.oneOf(allowedRegs).ensureNot(RiscvReg.ZERO)
          src <- Gen.oneOf(allowedRegs)
          base <- Gen.oneOf(allowedRegs).ensureNot(RiscvReg.ZERO).ensureNot(src)
          locationMove <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rdGen=Gen.lift(base), immGen=Gen.range(0, 1000))
          store <- RiscvMem.SW.makeGen(rs1Gen=Gen.lift(base), rs2Gen=Gen.lift(src), immGen=Gen.lift(0))
          // Am I supposed to put instructions here?
          load <- RiscvMem.LW.makeGen(rs1Gen=Gen.lift(base), rdGen=Gen.lift(dest), immGen=Gen.lift(0))
      } yield (Seq(locationMove, store, load))

      case ForLoop(body, count) => 
        for {
          counterRegIdx <- Gen.range(0, allowedRegs.length)
          counterReg <- Gen.lift(allowedRegs(counterRegIdx))
          mov <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rdGen=Gen.lift(counterReg), immGen=Gen.lift(count))
          b <- genInstrsForSeq(body, allowedRegs.patch(counterRegIdx, Nil, 1))
          j <- RiscvJump.J.makeGen(immGen=Gen.lift(- 2 * b.length))
          check <- RiscvBranch.BGE.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rs2Gen=Gen.lift(counterReg), immGen=Gen.lift(2 * (b.length + 1)))
        } yield (Seq(check) ++ b ++ Seq(j))

      case WhileLoop(body, cond) =>
        for {
          c <- cond.makeGen()
          b <- genInstrsForSeq(body, allowedRegs)
          j <- RiscvJump.JAL.makeGen(rdGen = Gen.lift(RiscvReg.ZERO), immGen = Gen.lift(- b.length * 2))
        } yield(Seq(c) ++ b)

      case PrimitiveInstruction(operator) => 
        for {
          instr <- operator.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), rdGen = Gen.oneOf(allowedRegs))
        } yield (Seq(instr))

      case BranchSequence(basicBlock1, basicBlock2, cond) => 
        for {
          bb1 <- genInstrsForSeq(basicBlock1, allowedRegs)
          bb2 <- genInstrsForSeq(basicBlock2, allowedRegs)
          c <- cond.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), immGen = Gen.lift(bb1.length * 2))
        } yield (Seq(c) ++ bb1 ++ bb2)

  def generationTrial(random: RNG): Unit =
    val nSeqs = 4
    val sequenceGenerator = Gen.seqToGen(Seq.fill(nSeqs)(gen()))
    // sequenceGenerator.generate(random).foreach(println(_))
    val y = sequenceGenerator.generate(random)(0)
    println(y)
    val z = genInstrsForSeq(y).generate(random)
    println(z)
    val instructions = 
      sequenceGenerator.flatMap(instrSequences => Gen.seqToGen(instrSequences.map(is => genInstrsForSeq(is))).map(_.flatten))
    println("-"*30)
    val instSeq = instructions.generate(random).foreach(println(_))

  def main(args: Array[String]): Unit =
    val is = List(2, 94, 6509, 347)

    is.foreach { i =>
      println(s"Run $i")
      println("-"*30)
      generationTrial(ParametricRandom.fromSeed(i))
    }


