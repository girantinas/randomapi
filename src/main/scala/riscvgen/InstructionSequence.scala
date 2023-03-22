package riscvgen
import randomapi.{Gen, RNG}
import randomapi.Gen.*
import RiscvMem.*
import RiscvOperator.*
import RiscvJump.*
import RiscvBranch.*
import riscvgen.{RiscvBranch, RiscvOperator, RiscvInstruction, RiscvMem, RiscvJump, LABEL}
import RiscvInstruction.RegState

import riscvgen.DecisionType.{Structure, Instruction, Immediate, Register}

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
  val MinAddressLong: Long = 0x80004000
  val MinAddress: BigInt = BigInt(MinAddressLong)
  val AddressSpaceSize: Long = 1000
  val MaxAddress: BigInt = MinAddress + AddressSpaceSize
  val WordSize = 4

  extension (self: RegState) def isLegalAccess(reg: RiscvReg): Boolean = {
    val v = self.get(reg)
    (v % WordSize == 0) && (v >= MinAddress) && (v <= MaxAddress)
  }

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
    genInstrsForSeqHelper(instrseq, 
                          RiscvReg.values.toIndexedSeq, 
                          RegState(m = Map.from(RiscvReg.values.zip(List.fill(RiscvReg.values.length)(BigInt(0))))), 
                          sequenceNumber, 1).map(p => p(0))

  def genInstrsForSeqHelper(instrseq: InstructionSequence, allowedRegs: Seq[RiscvReg], regState: RegState, sequenceNumber: Int, labelIdx: Int): Gen[(Seq[String], Int, RegState)] =
    if (allowedRegs.length == 1) then lift((Seq.empty, labelIdx, regState)) else
    instrseq match  
      case ReadAfterWrite(body) =>
        for {
          dest <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs).mark(Register)
          src <- Gen.oneOf(allowedRegs).mark(Register)
          base <- RiscvInstruction.nonzeroRiscvRegGen(allowedRegs).ensureNot(src).mark(Register)

          legal1 <- lift(regState.isLegalAccess(base))
          imm <- if legal1 then lift(BigInt(0)) else Gen.range(0, (AddressSpaceSize / 4).toInt).map(BigInt(_) * 4 + MinAddress).mark(Immediate)
          lMove1 <- if legal1 then lift("") else LI.makeGen(rdGen=lift(base), immGen=lift(imm))
          store <- SW.makeGen(rs1Gen=lift(base), rs2Gen=lift(src), immGen=lift(0))
          
          newRegState <- lift(if legal1 then regState else regState.update(base, imm))
          b <- genInstrsForSeqHelper(body, allowedRegs, newRegState, sequenceNumber, labelIdx)
          legal2 <- lift(b(2).isLegalAccess(base))
          lMove2 <- if legal2 then lift("") 
            else if legal1 then LI.makeGen(rdGen=lift(base), immGen=lift(regState.get(base)))
            else LI.makeGen(rdGen=lift(base), immGen=lift(imm))
          load <- LW.makeGen(rs1Gen=lift(base), rdGen=lift(dest), immGen=lift(0))
          // assume memory here was not changed
          newRegState <- lift(b(2).update(src, regState.get(base)))
        } yield ((((if !legal1 then Seq(lMove1) else Seq.empty[String]) 
                    :+ store)
                    ++ b(0) 
                    ++ (if !legal2 then Seq(lMove2) else Seq.empty[String])) 
                    :+ load, b(1), newRegState)

      case ForLoop(body, count) => // Bug: RegState only captures the first iteration of the loop. Need some kind of list of vals?
        for {
          counterRegIdx <- Gen.range(1, allowedRegs.length).mark(Immediate)
          counterReg <- lift(allowedRegs(counterRegIdx))
          mov <- ADDI.makeGen(rs1Gen=lift(RiscvReg.ZERO), rdGen=lift(counterReg), immGen=lift(BigInt(count)))
          // Do not update register state since loop registers are not allowed anyways.
          b <- genInstrsForSeqHelper(body, allowedRegs.patch(counterRegIdx, Nil, 1), regState, sequenceNumber, labelIdx + 1) 
          sub <- ADDI.makeGen(rs1Gen=lift(counterReg), rdGen=lift(counterReg), immGen=lift(BigInt(-1)))
          j <- J.makeGen(label=s"loop_${sequenceNumber}_$labelIdx")
          check <- BGE.makeGen(rs1Gen=lift(RiscvReg.ZERO), rs2Gen=lift(counterReg), label=s"continue_${sequenceNumber}_$labelIdx")
          forloop <- LABEL(s"loop_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
        } yield (Seq(mov, forloop, check) ++ b(0) ++ Seq(sub, j, continue), b(1), b(2).update(counterReg, 0))

      case WhileLoop(body, cond) =>
        for {
          b <- genInstrsForSeqHelper(body, allowedRegs, regState, sequenceNumber, labelIdx + 1)
          c <- cond.makeGen(label=s"continue_${sequenceNumber}_$labelIdx")
          whileloop <- LABEL(s"while_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
          j <- J.makeGen(label=s"while_${sequenceNumber}_$labelIdx")
        } yield(Seq(whileloop, c) ++ b(0) ++ Seq(j, continue), b(1), b(2))

      case PrimitiveInstruction(operator) => 
        for {
          rs1 <- Gen.oneOf(allowedRegs).mark(Register)
          rs2 <- Gen.oneOf(allowedRegs).mark(Register)
          rd <- Gen.oneOf(allowedRegs).mark(Register)
          instr <- operator.makeGenAndState(rs1Gen = Gen.lift(rs1), rs2Gen = Gen.lift(rs2), rdGen = Gen.lift(rd), state = regState)
        } yield (Seq(instr(0)), labelIdx, instr(1))

      case BranchSequence(basicBlock1, basicBlock2, cond) => 
        for {
          bb1 <- genInstrsForSeqHelper(basicBlock1, allowedRegs, regState, sequenceNumber, labelIdx + 1)
          bb2 <- genInstrsForSeqHelper(basicBlock2, allowedRegs, bb1(2), sequenceNumber, bb1(1))
          j <- J.makeGen(label=s"continue_${sequenceNumber}_$labelIdx")
          rs1 <- Gen.oneOf(allowedRegs).mark(Register)
          rs2 <- Gen.oneOf(allowedRegs).mark(Register)
          c <- cond.makeGen(rs1Gen = Gen.lift(rs1), rs2Gen = Gen.lift(rs2), label=s"then_${sequenceNumber}_$labelIdx")
          thenbranch <- LABEL(s"then_${sequenceNumber}_$labelIdx").makeGen()
          continue <- LABEL(s"continue_${sequenceNumber}_$labelIdx").makeGen()
        } yield (Seq(c) ++ bb1(0) ++ Seq(j, thenbranch) ++ bb2(0) ++ Seq(continue), bb2(1), bb2(2))
