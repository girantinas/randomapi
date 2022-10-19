package riscvgen

class RiscvInstr(group: RiscvInstrGroup,
                format: RiscvInstrFormat,
                category: RiscvInstrCategory,
                name: RiscvInstrName,
                immediateType: ImmediateType) {

  def setImmediateLength() = ???

  def setRandomMode() = ??? 
  
}


val group = RiscvInstrGroup.RV32I