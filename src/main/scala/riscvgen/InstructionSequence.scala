package riscvgen
import randomapi.{Gen, RNG}


enum InstructionSequenceType:
  case PrimitiveInstruction 
  case ReadAfterWrite
  case ForLoop
  case BranchSequence
  case WhileLoop

enum InstructionSequence:
   // PrimitiveInstruction: Wraps some Instruction type
  case PrimitiveInstruction(operator: RiscvOperator)
  case ReadAfterWrite(body: InstructionSequence)
  case ForLoop(body: InstructionSequence, count: Int)
  // For now: assume everything is a basic block.
  case BranchSequence(basicBlock1: InstructionSequence, basicBlock2: InstructionSequence, cond: RiscvBranch)
  case WhileLoop(body: InstructionSequence, cond: RiscvBranch)

object InstructionSequence:
  // Could be made easier with reflection?
  val MaxDepth = 10
  val InstructionSequenceBiases = {(depth: Int) => 
    val ProbPrimitive = depth.toDouble / MaxDepth
    // Currently excluding while loops
    val ProbNonPrimitive = (1 - depth.toDouble / MaxDepth) / (InstructionSequenceType.values.length - 2)
    ProbPrimitive +: Seq.fill(InstructionSequenceType.values.length - 2)(ProbNonPrimitive) :+ 0d
    }
  val MinAddress: Long = 0x80004000
  val AddressSpaceSize = 1000
  val WordSize = 4

  def gen(depth: Int = 0): Gen[InstructionSequence] = 
    for {
      sequenceType <- Gen.oneOf(InstructionSequenceType.values.toIndexedSeq, biases = Some(InstructionSequenceBiases(depth)))
      sequence <- {sequenceType match
        case InstructionSequenceType.ReadAfterWrite => for {
          body <- gen(depth + 1)
        } yield (ReadAfterWrite(body))

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

  def genInstrsForSeq(instrseq: InstructionSequence, sequenceNumber: Int): Gen[Seq[String]] =
    genInstrsForSeqHelper(instrseq, RiscvReg.values.toIndexedSeq, sequenceNumber, 1).map(p => p(0))

  def genInstrsForSeqHelper(instrseq: InstructionSequence, allowedRegs: Seq[RiscvReg], sequenceNumber: Int, labelIdx: Int): Gen[(Seq[String], Int)] =
    if (allowedRegs.length == 1) then Gen.lift((Seq.empty, labelIdx)) else
    instrseq match  
      case ReadAfterWrite(body) =>
        for {
          dest <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs)
          src <- Gen.oneOf(allowedRegs)
          base <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs).ensureNot(src)
          locationMove <- RiscvOperator.LI.makeGen(rdGen=Gen.lift(base), immGen=Gen.range(0, AddressSpaceSize / 4).map(MinAddress + _.toLong * 4))
          store <- RiscvMem.SW.makeGen(rs1Gen=Gen.lift(base), rs2Gen=Gen.lift(src), immGen=Gen.lift(0))
          b <- genInstrsForSeqHelper(body, allowedRegs, sequenceNumber, labelIdx)
          load <- RiscvMem.LW.makeGen(rs1Gen=Gen.lift(base), rdGen=Gen.lift(dest), immGen=Gen.lift(0))
      } yield ( (Seq(locationMove, store) ++ b(0) ++ Seq(locationMove, load), b(1)) )

      case ForLoop(body, count) => 
        for {
          counterRegIdx <- Gen.range(1, allowedRegs.length)
          counterReg <- Gen.lift(allowedRegs(counterRegIdx))
          mov <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rdGen=Gen.lift(counterReg), immGen=Gen.lift(count))
          b <- genInstrsForSeqHelper(body, allowedRegs.patch(counterRegIdx, Nil, 1), sequenceNumber, labelIdx + 1)
          sub <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(counterReg), rdGen=Gen.lift(counterReg), immGen=Gen.lift(-1))
          j <- RiscvJump.J.makeGen(label=s"loop_${sequenceNumber}_$labelIdx")
          check <- RiscvBranch.BGE.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rs2Gen=Gen.lift(counterReg), label=s"continue_${sequenceNumber}_$labelIdx")
          forloop <- LABEL(s"loop_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
        } yield ((Seq(mov, forloop, check) ++ b(0) ++ Seq(sub, j, continue), b(1)))

      case WhileLoop(body, cond) =>
        for {
          b <- genInstrsForSeqHelper(body, allowedRegs, sequenceNumber, labelIdx + 1)
          c <- cond.makeGen(label=s"continue_${sequenceNumber}_$labelIdx")
          whileloop <- LABEL(s"while_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
          j <- RiscvJump.J.makeGen(label=s"while_${sequenceNumber}_$labelIdx")
        } yield((Seq(whileloop, c) ++ b(0) ++ Seq(j, continue), b(1)))

      case PrimitiveInstruction(operator) => 
        for {
          instr <- operator.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), rdGen = Gen.oneOf(allowedRegs))
        } yield ((Seq(instr), labelIdx))

      case BranchSequence(basicBlock1, basicBlock2, cond) => 
        for {
          bb1 <- genInstrsForSeqHelper(basicBlock1, allowedRegs, sequenceNumber, labelIdx + 1)
          bb2 <- genInstrsForSeqHelper(basicBlock2, allowedRegs, sequenceNumber, bb1(1))
          j <- RiscvJump.J.makeGen(label=s"continue_${sequenceNumber}_$labelIdx")
          c <- cond.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), label=s"then_${sequenceNumber}_$labelIdx")
          thenbranch <- LABEL(s"then_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
        } yield ((Seq(c) ++ bb1(0) ++ Seq(j, thenbranch) ++ bb2(0) ++ Seq(continue), bb2(1)))
