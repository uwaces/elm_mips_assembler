with open("program.txt") as file:
        byte = 0
        format_dmem = '      imemBytes({})  <= "{}";'
        for line in file:
                print(format_dmem.format(byte, line[0:8]))
                print(format_dmem.format(byte+1, line[8:16]))
                print(format_dmem.format(byte+2, line[16:24]))
                print(format_dmem.format(byte+3, line[24:32]))
                byte+= 4
