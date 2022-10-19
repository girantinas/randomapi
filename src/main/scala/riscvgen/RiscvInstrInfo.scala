package riscvgen

enum RiscvInstrGroup {
  case RV32I
  case RV64I
  case RV32M
  case RV64M
  case RV32A 
  case RV64A 
  case RV32F
  case RV32FC
  case RV64F
  case RV32D
  case RV32DC
  case RV64D
  case RV32C
  case RV64C
  case RV128I
  case RV128C
  case RV32V
  case RV32B
  case RV64V
  case RV64B
  case RV32X
  case RV64X
  case RVV
}

enum RiscvInstrName {
  // Only RV32I instructions for now
  case LUI
  case AUIPC
  case JAL
  case JALR
  case BEQ
  case BNE
  case BLT
  case BGE
  case BLTU
  case BGEU
  case LB
  case LH
  case LW
  case LBU
  case LHU
  case SB
  case SH
  case SW
  case ADDI
  case SLTI
  case SLTIU
  case XORI
  case ORI
  case ANDI
  case SLLI
  case SRLI
  case SRAI
  case ADD
  case SUB
  case SLL
  case SLT
  case SLTU
  case XOR
  case SRL
  case SRA
  case OR
  case AND
  case NOP
  case FENCE
  case FENCE_I
  case ECALL
  case EBREAK
  case CSRRW
  case CSRRS
  case CSRRC
  case CSRRWI
  case CSRRSI
  case CSRRCI
}

enum RiscvInstrCategory {
  case LOAD
  case STORE
  case SHIFT
  case ARITHMETIC
  case LOGICAL
  case COMPARE
  case BRANCH
  case JUMP
  case SYNCH
  case SYSTEM
  case COUNTER
  case CSR
  case CHANGELEVEL
  case TRAP
  case INTERRUPT
  case AMO
}

enum RiscvInstrFormat {
  case J_FORMAT
  case U_FORMAT
  case I_FORMAT
  case B_FORMAT
  case R_FORMAT
  case S_FORMAT
  case R4_FORMAT
  // Compressed instruction format
  case CI_FORMAT
  case CB_FORMAT
  case CJ_FORMAT
  case CR_FORMAT
  case CA_FORMAT
  case CL_FORMAT
  case CS_FORMAT
  case CSS_FORMAT
  case CIW_FORMAT
  // Vector formats
  case VSET_FORMAT
  case VA_FORMAT
  case VS2_FORMAT
  case VL_FORMAT
  case VS_FORMAT
}

enum ImmediateType {
  case IMM // Signed immediate
  case UIMM // Unsigned immediate
  case NZUIMM // Non-zero unsigned immediate
  case NZIMM // Non-zero signed immediate
}

enum RiscvReg {
  case ZERO
  case RA
  case SP
  case GP
  case TP
  case T0
  case T1
  case T2
  case S0
  case S1
  case A0
  case A1
  case A2
  case A3
  case A4
  case A5
  case A6
  case A7
  case S2
  case S3
  case S4
  case S5
  case S6
  case S7
  case S8
  case S9
  case S10
  case S11
  case T3
  case T4
  case T5
  case T6
}