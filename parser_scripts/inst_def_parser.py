
# %%
with open('./inst_data', 'r') as f:
    data = f.read()
    lines = data.split('\n')[1:]

# %%
lst = [*filter(lambda x: x != '' and x[0] != '#', lines)]
idxs = [i for i, elem in enumerate(lst) if (i != len(lst) - 1) and elem.startswith('DEFINE_INSTR')]
define_directives = [''.join((lst[idx] + lst[idx + 1]).split()) for idx in idxs]
# %%
import re
pattern = r'DEFINE_INSTR\(riscv_instr_name_t\.(\w+),riscv_instr_format_t\.(\w+),riscv_instr_category_t\.(\w+),riscv_instr_group_t\.(\w+)'
imm_pattern = r'imm_t\.(\w+)'
with open('./def_data_out', 'w+') as g:
    for direc in define_directives:
        main_groups = re.match(pattern, direc)
        imm_match = re.search(imm_pattern, direc)
        inst_name = main_groups.group(1)
        inst_format = main_groups.group(2)
        inst_cat = main_groups.group(3)
        inst_group = main_groups.group(4)
        inst_imm_t = imm_match.group(1) if imm_match else 'IMM'
        g.write(f'case {inst_name} extends RV32iInstr with RiscvInstruction(group = RiscvInstrGroup.{inst_group}, format = RiscvInstrFormat.{inst_format}, category = RiscvInstrCategory.{inst_cat}, immediateType = ImmediateType.{inst_imm_t})\n')
        

# %%

# %%
