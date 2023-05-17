#=======================================================================
# Makefile for riscv-tests/isa
#-----------------------------------------------------------------------

src_dir := .
XLEN := 64

include $(src_dir)/asm/Makefrag

default: all

#--------------------------------------------------------------------
# Build rules
#--------------------------------------------------------------------

RISCV_PREFIX ?= riscv$(XLEN)-unknown-elf-
RISCV_GCC ?= $(RISCV_PREFIX)gcc
RISCV_GCC_OPTS ?= -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles
RISCV_SIM ?= spike

vpath %.S $(src_dir)

#------------------------------------------------------------
# Build assembly tests

$(targets): %: asm/%.S
	$(RISCV_GCC) -march=rv64g $(RISCV_GCC_OPTS) -I$(src_dir)/../env/p -I$(src_dir)/macros/scalar -T$(src_dir)/../env/p/link.ld $< -o $@ -O0

#------------------------------------------------------------
# Default

all: $(targets)

#------------------------------------------------------------
# Clean up

clean:
	rm -rf $(tests)
