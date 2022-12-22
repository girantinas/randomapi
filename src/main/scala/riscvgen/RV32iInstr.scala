package riscvgen
import randomapi.Gen
import randomapi.Gen.*

enum RV32iInstr:
  case LUI(rd: RiscvReg, imm20: IMM20)
  case AUIPC(rd: RiscvReg, imm20: IMM20)
  case JAL(rd: RiscvReg, jimm20: JIMM20)
  case JALR(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case BEQ(bimm12hi: BIMM12HI, rs1: RiscvReg, rs2: RiscvReg, bimm12lo: BIMM12LO)
  case BNE(bimm12hi: BIMM12HI, rs1: RiscvReg, rs2: RiscvReg, bimm12lo: BIMM12LO)
  case BLT(bimm12hi: BIMM12HI, rs1: RiscvReg, rs2: RiscvReg, bimm12lo: BIMM12LO)
  case BGE(bimm12hi: BIMM12HI, rs1: RiscvReg, rs2: RiscvReg, bimm12lo: BIMM12LO)
  case BLTU(bimm12hi: BIMM12HI, rs1: RiscvReg, rs2: RiscvReg, bimm12lo: BIMM12LO)
  case BGEU(bimm12hi: BIMM12HI, rs1: RiscvReg, rs2: RiscvReg, bimm12lo: BIMM12LO)
  case LB(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case LH(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case LW(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case LBU(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case LHU(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case SB(imm12hi: IMM12HI, rs1: RiscvReg, rs2: RiscvReg, imm12lo: IMM12LO)
  case SH(imm12hi: IMM12HI, rs1: RiscvReg, rs2: RiscvReg, imm12lo: IMM12LO)
  case SW(imm12hi: IMM12HI, rs1: RiscvReg, rs2: RiscvReg, imm12lo: IMM12LO)
  case ADDI(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case SLTI(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case SLTIU(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case XORI(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case ORI(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case ANDI(rd: RiscvReg, rs1: RiscvReg, imm12: IMM12)
  case ADD(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case SUB(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case SLL(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case SLT(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case SLTU(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case XOR(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case SRL(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case SRA(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case OR(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)
  case AND(rd: RiscvReg, rs1: RiscvReg, rs2: RiscvReg)

sealed trait RiscvInstruction

sealed trait PrimitiveRiscvInstruction extends RiscvInstruction

object RiscvInstruction:
  def immTypeGen(immType: ImmediateType): Gen[Long] =
    immType match
      case ImmediateType.IMM(n) => Gen.nBitSignedInt(n).map(_.toLong)
      case ImmediateType.UIMM(n) => Gen.nBitUnsignedInt(n).map(_.toLong)

  def nonzeroRiscvRegGen(allowedRegs: Seq[RiscvReg]): Gen[RiscvReg] = 
    if allowedRegs(0) == RiscvReg.ZERO 
    then Gen.oneOf(allowedRegs, biases = Some(Seq.fill(allowedRegs.length)(1d).patch(0, Seq(0d), 1)))
    else Gen.oneOf(allowedRegs)


enum RiscvOperator(hasRs1: Boolean,
                           hasRs2: Boolean,
                           hasRd: Boolean,
                           hasImmediate: Boolean,
                           immType: ImmediateType = ImmediateType.IMM(12))
                           (toString: (RiscvReg, RiscvReg, RiscvReg, Long) => String) extends RiscvInstruction:
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rs2Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rdGen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Long] = RiscvInstruction.immTypeGen(immType)): Gen[String] =
    for {
      rs1 <- if (hasRs1) then rs1Gen else Gen.lift(RiscvReg.ZERO)
      rs2 <- if (hasRs2) then rs2Gen else Gen.lift(RiscvReg.ZERO)
      rd <- if (hasRd) then rdGen else Gen.lift(RiscvReg.ZERO)
      imm <- if (hasImmediate) then immGen else Gen.lift(0L)
    } yield (toString(rs1, rs2, rd, imm))
  
  // Arithmetic
  case ADD extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"add ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case SUB extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sub ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case SLL extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sll ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case SLT extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"slt ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case SLTU extends  RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sltu ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case XOR extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"xor ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case SRL extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"srl ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case SRA extends  RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sra ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case OR extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"or ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction
  case AND extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"and ${rd.toAsmString()}, ${rs1.toAsmString()}, ${rs2.toAsmString()}") with PrimitiveRiscvInstruction

  // Arithmetic with Register + Immediates
  case ADDI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"addi ${rd.toAsmString()}, ${rs1.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  case SLTI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"slti ${rd.toAsmString()}, ${rs1.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  case SLTIU extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"sltiu ${rd.toAsmString()}, ${rs1.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  case XORI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"xori ${rd.toAsmString()}, ${rs1.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  case ORI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"ori ${rd.toAsmString()}, ${rs1.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  case ANDI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"andi ${rd.toAsmString()}, ${rs1.toAsmString()}, $imm") with PrimitiveRiscvInstruction

  // Arithmetic with Just Immediates
  case NOP extends RiscvOperator(false, false, false, false)((_, _, _, _) => "nop") with PrimitiveRiscvInstruction
  case LUI extends RiscvOperator(true, false, true, true, immType = ImmediateType.UIMM(20))((rs1, rs2, rd, imm) => s"lui ${rd.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  case LI extends RiscvOperator(false, false, true, true)((rs1, rs2, rd, imm) => s"li ${rd.toAsmString()}, $imm") with PrimitiveRiscvInstruction
  // case AUIPC extends RiscvOperator(false, false, true, true, immType = ImmediateType.UIMM(20))((rs1, rs2, rd, imm) => s"auipc ${rd.toAsmString()}, $imm") with PrimitiveRiscvInstruction

enum RiscvJump(hasRs1: Boolean, hasOffset: Boolean)(toString: (RiscvReg, RiscvReg, Long, String) => String) extends RiscvInstruction:
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rdGen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Long] = RiscvInstruction.immTypeGen(ImmediateType.IMM(12)),
              label: String): Gen[String] =
    for {
      rs1 <- if (hasRs1) then rs1Gen else Gen.lift(RiscvReg.ZERO)
      rd <- rdGen
      offset <- if (hasOffset) then immGen else Gen.lift(0L)
    } yield (toString(rs1, rd, offset, label))

  case JALR extends RiscvJump(true, true)((rs1, rd, offset, _) => s"jalr ${rd.toAsmString()}, $offset(${rs1.toAsmString()})")
  case JAL extends RiscvJump(false, false)((_, rd, _, label) => s"jal ${rd.toAsmString()}, $label")
  case J extends RiscvJump(false, false)((_, _, _, label) => s"jal x0, $label")

enum RiscvBranch(toString: (RiscvReg, RiscvReg, String) => String):
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rs2Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              label: String): Gen[String] =
    for {
      rs1 <- rs1Gen
      rs2 <- rs2Gen
    } yield (toString(rs1, rs2, label))

  case BEQ extends RiscvBranch((rs1, rs2, label) => s"beq ${rs1.toAsmString()}, ${rs2.toAsmString()}, $label")
  case BNE extends RiscvBranch((rs1, rs2, label) => s"bne ${rs1.toAsmString()}, ${rs2.toAsmString()}, $label")
  case BGE extends RiscvBranch((rs1, rs2, label) => s"bge ${rs1.toAsmString()}, ${rs2.toAsmString()}, $label")
  case BLT extends RiscvBranch((rs1, rs2, label) => s"blt ${rs1.toAsmString()}, ${rs2.toAsmString()}, $label")
  case BLTU extends RiscvBranch((rs1, rs2, label) => s"bltu ${rs1.toAsmString()}, ${rs2.toAsmString()}, $label")
  case BGEU extends RiscvBranch((rs1, rs2, label) => s"bgeu ${rs1.toAsmString()}, ${rs2.toAsmString()}, $label")

enum RiscvMem(hasRs1: Boolean, hasRs2: Boolean, hasRd: Boolean, hasImmediate: Boolean)(toString: (RiscvReg, RiscvReg, RiscvReg, Long) => String):
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rs2Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rdGen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Long] = RiscvInstruction.immTypeGen(ImmediateType.IMM(12))): Gen[String] =     
    for {
      rs1 <- if (hasRs1) then rs1Gen else Gen.lift(RiscvReg.ZERO)
      rs2 <- if (hasRs2) then rs2Gen else Gen.lift(RiscvReg.ZERO)
      rd <- if (hasRd) then rdGen else Gen.lift(RiscvReg.ZERO)
      imm <- if (hasImmediate) then immGen else Gen.lift(0L)
    } yield (toString(rs1, rs2, rd, imm))
  
  case LW extends RiscvMem(true, false, true, true)((rs1, _, rd, offset) => s"lw ${rd.toAsmString()}, $offset(${rs1.toAsmString()})")
  case SW extends RiscvMem(true, true, false, true)((rs1, rs2, _, offset) => s"sw ${rs2.toAsmString()}, $offset(${rs1.toAsmString()})")


case class LABEL(label: String):
  def makeGen() = Gen.lift(s"$label:")