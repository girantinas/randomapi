with open('./enum_data', 'r') as f:
    data = f.read()
    lines = data.split('\n')[1:]
    print(lines)
    final = '\n'.join(["case " + line.split(' ')[4] for line in lines])
    with open('./enum_data_out', 'w+') as g:
        g.write(final)