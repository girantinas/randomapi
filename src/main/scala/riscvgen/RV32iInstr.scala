package riscvgen
import randomapi.Gen
import randomapi.Gen.*

enum ArithemeticOperator(hasRs1: Boolean, hasRs2: Boolean, hasRd: Boolean, hasImmediate: Boolean, immediateType: ImmediateType = ImmediateType.IMM)(toString: (RiscvReg, RiscvReg, RiscvReg, Int) => String):
  def makeGen(): Gen[String] =
    for {
      rs1 <- if (hasRs1) then Gen.oneOf(RiscvReg.values) else Gen.lift(RiscvReg.ZERO)
      rs2 <- if (hasRs2) then Gen.oneOf(RiscvReg.values) else Gen.lift(RiscvReg.ZERO)
      rd <- if (hasRd) then Gen.oneOf(RiscvReg.values) else Gen.lift(RiscvReg.ZERO)
      imm <- if (hasImmediate) then {
        if (immediateType == ImmediateType.IMM) then Gen.int
        else Gen.nonNegativeInt
      } else Gen.lift(0)
    } yield (toString(rs1, rs2, rd, imm))

  case ADD extends ArithemeticOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"add $rd, $rs1, $rs2")
  case ADDI extends ArithemeticOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"addi $rd, $rs1, $imm")
  case NOP extends ArithemeticOperator(false, false, false, false)((_, _, _, _)=> "nop")
  case SUB extends ArithemeticOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sub $rd, $rs1, $rs2")
  case LUI extends ArithemeticOperator(true, false, false, true)((rs1, rs2, rd, imm) => s"lui $rd, $imm")
  case AUIPC extends ArithemeticOperator(true, false, false, true)((rs1, rs2, rd, imm) => s"auipc $rd, $imm")


  // Gen[Gen[T]]
  // RISC-V Instruction Type
  // 
  // Gen[ADD]
  // Gen.oneOF(instrs)