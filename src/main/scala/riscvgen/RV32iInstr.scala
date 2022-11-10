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
  def immTypeGen(immType: ImmediateType): Gen[Int] =
    immType match
      case ImmediateType.IMM(n) => Gen.nBitSignedInt(n)
      case ImmediateType.UIMM(n) => Gen.nBitUnsignedInt(n)

  def nonzeroRiscvRegGen(): Gen[RiscvReg] = 
    val numRegs = RiscvReg.values.length
    Gen.oneOf(RiscvReg.values, Some(Seq.fill(numRegs)(1d).patch(0, Seq(0d), 1)))

enum RiscvOperator(hasRs1: Boolean,
                           hasRs2: Boolean,
                           hasRd: Boolean,
                           hasImmediate: Boolean)
                           (toString: (RiscvReg, RiscvReg, RiscvReg, Int) => String) extends RiscvInstruction:
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rs2Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rdGen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Int] = Gen.nBitSignedInt(12)): Gen[String] =
    for {
      rs1 <- if (hasRs1) then rs1Gen else Gen.lift(RiscvReg.ZERO)
      rs2 <- if (hasRs2) then rs2Gen else Gen.lift(RiscvReg.ZERO)
      rd <- if (hasRd) then rdGen else Gen.lift(RiscvReg.ZERO)
      imm <- if (hasImmediate) then immGen else Gen.lift(0)
    } yield (toString(rs1, rs2, rd, imm))
  
  // Arithmetic
  case ADD extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"add ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case SUB extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sub ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case SLL extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sll ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case SLT extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"slt ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case SLTU extends  RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sltu ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case XOR extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"xor ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case SRL extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"srl ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case SRA extends  RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"sra ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case OR extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"or ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction
  case AND extends RiscvOperator(true, true, true, false)((rs1, rs2, rd, imm) => s"and ${rd.toString()}, ${rs1.toString()}, ${rs2.toString()}") with PrimitiveRiscvInstruction

  // Arithmetic with Register + Immediates
  case ADDI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"addi ${rd.toString()}, ${rs1.toString()}, $imm") with PrimitiveRiscvInstruction
  case SLTI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"slti ${rd.toString()}, ${rs1.toString()}, $imm") with PrimitiveRiscvInstruction
  case SLTIU extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"sltiu ${rd.toString()}, ${rs1.toString()}, $imm") with PrimitiveRiscvInstruction
  case XORI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"xori ${rd.toString()}, ${rs1.toString()}, $imm") with PrimitiveRiscvInstruction
  case ORI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"ori ${rd.toString()}, ${rs1.toString()}, $imm") with PrimitiveRiscvInstruction
  case ANDI extends RiscvOperator(true, false, true, true)((rs1, rs2, rd, imm) => s"andi ${rd.toString()}, ${rs1.toString()}, $imm") with PrimitiveRiscvInstruction

  // Arithmetic with Just Immediates
  case NOP extends RiscvOperator(false, false, false, false)((_, _, _, _) => "nop") with PrimitiveRiscvInstruction
  case LUI extends RiscvOperator(true, false, false, true)((rs1, rs2, rd, imm) => s"lui ${rd.toString()}, $imm") with PrimitiveRiscvInstruction
  case AUIPC extends RiscvOperator(true, false, false, true)((rs1, rs2, rd, imm) => s"auipc ${rd.toString()}, $imm") with PrimitiveRiscvInstruction

enum RiscvJump(hasRs1: Boolean, immType: ImmediateType)(toString: (RiscvReg, RiscvReg, Int) => String) extends RiscvInstruction:
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rdGen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Int] = RiscvInstruction.immTypeGen(immType)): Gen[String] =
    for {
      rs1 <- if (hasRs1) then rs1Gen else Gen.lift(RiscvReg.ZERO)
      rd <- rdGen
      offset <- immGen
    } yield (toString(rs1, rd, offset))

  case JALR extends RiscvJump(true, ImmediateType.IMM(12))((rs1, rd, offset) => s"jalr ${rd.toString()}, $offset(${rs1.toString()})")
  case JAL extends RiscvJump(false, ImmediateType.IMM(20))((_, rd, offset) => s"jal ${rd.toString()}, $offset")
  case J extends RiscvJump(false, ImmediateType.IMM(20))((_, _, offset) => s"jal x0, $offset")

enum RiscvBranch(toString: (RiscvReg, RiscvReg, Int) => String):
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rs2Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Int] = RiscvInstruction.immTypeGen(ImmediateType.IMM(12))): Gen[String] =
    for {
      rs1 <- rs1Gen
      rs2 <- rs2Gen
      offset <- immGen
    } yield (toString(rs1, rs2, offset))

  case BEQ extends RiscvBranch((rs1, rs2, offset) => s"beq ${rs1.toString()}, ${rs2.toString()}, $offset")
  case BNE extends RiscvBranch((rs1, rs2, offset) => s"bne ${rs1.toString()}, ${rs2.toString()}, $offset")
  case BGE extends RiscvBranch((rs1, rs2, offset) => s"bge ${rs1.toString()}, ${rs2.toString()}, $offset")
  case BLT extends RiscvBranch((rs1, rs2, offset) => s"blt ${rs1.toString()}, ${rs2.toString()}, $offset")
  case BLTU extends RiscvBranch((rs1, rs2, offset) => s"bltu ${rs1.toString()}, ${rs2.toString()}, $offset")
  case BGEU extends RiscvBranch((rs1, rs2, offset) => s"bgeu ${rs1.toString()}, ${rs2.toString()}, $offset")

enum RiscvMem(hasRs1: Boolean, hasRs2: Boolean, hasRd: Boolean, hasImmediate: Boolean)(toString: (RiscvReg, RiscvReg, RiscvReg, Int) => String):
  def makeGen(rs1Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rs2Gen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              rdGen: Gen[RiscvReg] = Gen.oneOf(RiscvReg.values),
              immGen: Gen[Int] = RiscvInstruction.immTypeGen(ImmediateType.IMM(12))): Gen[String] =     
    for {
      rs1 <- if (hasRs1) then rs1Gen else Gen.lift(RiscvReg.ZERO)
      rs2 <- if (hasRs2) then rs2Gen else Gen.lift(RiscvReg.ZERO)
      rd <- if (hasRd) then rdGen else Gen.lift(RiscvReg.ZERO)
      imm <- if (hasImmediate) then immGen else Gen.lift(0)
    } yield (toString(rs1, rs2, rd, imm))
  
  case LW extends RiscvMem(true, false, true, true)((rs1, _, rd, offset) => s"lw ${rd.toString()}, $offset(${rs1.toString()})")
  case SW extends RiscvMem(true, true, false, true)((rs1, rs2, _, offset) => s"sw ${rs2.toString()}, $offset(${rs1.toString()})")