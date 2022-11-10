
# %%
with open('./opcode_data', 'r') as f:
    data = f.read()
    lines = data.split('\n')

# %%
import itertools
instr_descs = [[*filter(lambda x: not x[0].isnumeric(), line.split())] for line in lines]
with open('./opcode_data_out', 'w+') as g:
    for instr_desc in instr_descs:
        class_string = f'case {instr_desc[0].upper()}('
        for i, elem in enumerate(instr_desc[1:]):
            if elem in ['rd', 'rs1', 'rs2']:
                class_string += elem + ': ' + 'RiscvReg, '
            else:
                class_string += elem + ': ' + elem.upper() + ', '
        if class_string[-2:] == ', ':
            class_string = class_string[:-2]
        class_string += ')' # + ' extends RV32iInstr'
        g.write(f'{class_string}\n')


# %%
import itertools
instr_descs = [[*filter(lambda x: not x[0].isnumeric(), line.split())] for line in lines]
with open('./opcode_data_out_abstract', 'w+') as g:
    for instr_desc in instr_descs:
        class_string = f'case {instr_desc[0].upper()}'
        for i, elem in enumerate(instr_desc[1:]):
            if elem in ['rd', 'rs1', 'rs2']:
                class_string += elem + ': ' + 'RiscvReg, '
            else:
                class_string += elem + ': ' + elem.upper() + ', '
        if class_string[-2:] == ', ':
            class_string = class_string[:-2]
        class_string += ')' # + ' extends RV32iInstr'
        g.write(f'{class_string}\n')