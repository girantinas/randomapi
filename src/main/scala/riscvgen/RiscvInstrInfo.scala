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

enum ImmediateType {
  case IMM(n: Int) // Signed immediate
  case UIMM(n: Int) // Unsigned immediate
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

object RiscvReg {
  extension [A] (self: RiscvReg) def toAsmString(): String = self match
    case ZERO => "x0"
    case RA => "ra"
    case SP => "sp"
    case GP => "gp"
    case TP => "tp"
    case T0 => "t0"
    case T1 => "t1"
    case T2 => "t2"
    case S0 => "s0"
    case S1 => "s1"
    case A0 => "a0"
    case A1 => "a1"
    case A2 => "a2"
    case A3 => "a3"
    case A4 => "a4"
    case A5 => "a5"
    case A6 => "a6"
    case A7 => "a7"
    case S2 => "s2"
    case S3 => "s3"
    case S4 => "s4"
    case S5 => "s5"
    case S6 => "s6"
    case S7 => "s7"
    case S8 => "s8"
    case S9 => "s9"
    case S10 => "s10"
    case S11 => "s11"
    case T3 => "t3"
    case T4 => "t4"
    case T5 => "t5"
    case T6 => "t6"
}