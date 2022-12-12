package riscvgen
import randomapi.Gen
import randomapi.RNG
import randomapi.ParametricRandom
import java.nio.file.{Paths, Files, StandardOpenOption}
import java.nio.charset.StandardCharsets

enum InstructionSequenceType:
  case PrimitiveInstruction 
  case ReadAfterWrite
  case ForLoop
  case WhileLoop
  case BranchSequence

enum InstructionSequence:
   // PrimitiveInstruction: Wraps some Instruction type
  case PrimitiveInstruction(operator: RiscvOperator)
  case ReadAfterWrite(body: InstructionSequence)
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
  val MinAddress: Long = 0x80000000
  val AddressSpaceSize = 1000

  def gen(depth: Int = 0): Gen[InstructionSequence] = 
    for {
      sequenceType <- Gen.oneOf(InstructionSequenceType.values, biases = Some(InstructionSequenceBiases(depth)))
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

  def genInstrsForSeq(instrseq: InstructionSequence): Gen[Seq[String]] =
    genInstrsForSeqHelper(instrseq, RiscvReg.values.toIndexedSeq, 0).map(p => p(0))

  def genInstrsForSeqHelper(instrseq: InstructionSequence, allowedRegs: Seq[RiscvReg], labelIdx: Int): Gen[(Seq[String], Int)] =
    if (allowedRegs.length == 1) then Gen.lift((Seq.empty, labelIdx)) else
    instrseq match  
      case ReadAfterWrite(body) =>
        for {
          dest <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs)
          src <- Gen.oneOf(allowedRegs)
          base <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs).ensureNot(src) // TODO: fix
          locationMove <- RiscvOperator.LI.makeGen(rdGen=Gen.lift(base), immGen=Gen.range(0, AddressSpaceSize).map(MinAddress + _.toLong))
          store <- RiscvMem.SW.makeGen(rs1Gen=Gen.lift(base), rs2Gen=Gen.lift(src), immGen=Gen.lift(0))
          b <- genInstrsForSeqHelper(body, allowedRegs, labelIdx)
          load <- RiscvMem.LW.makeGen(rs1Gen=Gen.lift(base), rdGen=Gen.lift(dest), immGen=Gen.lift(0))
      } yield ( (Seq(locationMove, store) ++ b(0) ++ Seq(load), b(1)) )

      case ForLoop(body, count) => 
        for {
          counterRegIdx <- Gen.range(0, allowedRegs.length)
          counterReg <- Gen.lift(allowedRegs(counterRegIdx))
          mov <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rdGen=Gen.lift(counterReg), immGen=Gen.lift(count))
          b <- genInstrsForSeqHelper(body, allowedRegs.patch(counterRegIdx, Nil, 1), labelIdx + 1)
          sub <- RiscvOperator.ADDI.makeGen(rs1Gen=Gen.lift(counterReg), rdGen=Gen.lift(counterReg), immGen=Gen.lift(-1))
          j <- RiscvJump.J.makeGen(label=s"loop_$labelIdx")
          check <- RiscvBranch.BGE.makeGen(rs1Gen=Gen.lift(RiscvReg.ZERO), rs2Gen=Gen.lift(counterReg), label=s"continue_$labelIdx")
          forloop <- LABEL(s"loop_$labelIdx").makeGen()
          continue <- LABEL(s"continue_$labelIdx").makeGen()
        } yield ((Seq(forloop, check) ++ b(0) ++ Seq(sub, j, continue), b(1)))

      case WhileLoop(body, cond) =>
        for {
          b <- genInstrsForSeqHelper(body, allowedRegs, labelIdx + 1)
          c <- cond.makeGen(label=s"continue_$labelIdx")
          whileloop <- LABEL(s"loop_$labelIdx").makeGen()
          continue <- LABEL(s"continue_$labelIdx").makeGen()
          j <- RiscvJump.J.makeGen(label=s"loop_$labelIdx")
        } yield((Seq(whileloop, c) ++ b(0) ++ Seq(j, continue), b(1)))

      case PrimitiveInstruction(operator) => 
        for {
          instr <- operator.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), rdGen = Gen.oneOf(allowedRegs))
        } yield ((Seq(instr), labelIdx))

      case BranchSequence(basicBlock1, basicBlock2, cond) => 
        for {
          bb1 <- genInstrsForSeqHelper(basicBlock1, allowedRegs, labelIdx + 1)
          bb2 <- genInstrsForSeqHelper(basicBlock2, allowedRegs, bb1(1))
          j <- RiscvJump.J.makeGen(label=s"continue_$labelIdx")
          c <- cond.makeGen(rs1Gen = Gen.oneOf(allowedRegs), rs2Gen = Gen.oneOf(allowedRegs), label=s"then_$labelIdx")
          thenbranch <- LABEL(s"then_$labelIdx").makeGen()
          continue <- LABEL(s"continue_$labelIdx").makeGen()
        } yield ((Seq(c) ++ bb1(0) ++ Seq(j, thenbranch) ++ bb2(0) ++ Seq(continue), bb2(1)))

  def generationTrial(i: Int): Unit =
    val random = ParametricRandom.fromSeed(i)
    println(s"\nRun $i")
    println("-"*30)
    val nSeqs = 5
    val sequenceGenerator = Gen.seqToGen(Seq.fill(nSeqs)(gen()))
    val instructions = 
      sequenceGenerator.flatMap(instrSequences => Gen.seqToGen(instrSequences.map(is => genInstrsForSeq(is))).map(_.flatten))
    val instSeq = instructions.generate(random)

    val prologue = "#include \"riscv_test.h\"\n#include \"test_macros.h\"\nRVTEST_RV64U\nRVTEST_CODE_BEGIN\n"
    val epilogue = "RVTEST_PASS\nRVTEST_CODE_END\n.data\nRVTEST_DATA_BEGIN\nTEST_DATA\nRVTEST_DATA_END\n"
    
    Files.write(Paths.get(s"test_files/out$i.S"), 
      (instSeq.foldLeft(prologue)((sofar, s) => sofar + s + '\n') + epilogue).getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE)

  def main(args: Array[String]): Unit =
    val is = List(2, 94, 6509, 347)

    is.foreach (generationTrial(_))


