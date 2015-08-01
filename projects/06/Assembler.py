import sys

class Parse:
    command = "\n"
    line = 0
    eof = False
    
    def __init__(self, file_name):
        try:
            self.f = open(file_name, "r")
        except IOError:
            print("Could not open {}", f)
            exit(1)
            

    def hasMoreCommands(self):
        return not self.eof

    def advance(self):
        try:
            self.command = self.f.readline().strip()
            self.line += 1
            #print("%r"%self.command)
            if self.command == "":
                self.eof = True
                return False
        except IOError:
            print("Could not read from specified file.")
            exit(1)
        
        while (self.command == "\n" or self.command.startswith("//")
               or self.command == "\r\n"):
            try:
               self.command = self.f.readline()
               self.line +=1
               if self.command == "":
                   self.eof = True
                   return False
            except IOError:
               print("Could not read from specified file.")
               exit(1)
            
        if self.command.find("//") > -1:
            self.command = self.command[0:self.command.find("//")].strip()
        else:
            self.command = self.command.strip()
        return True

    def commandType(self):
        if self.command.startswith("@"):
            return "A_COMMAND"
        elif self.command.startswith("("):
            return "L_COMMAMD"
        elif (self.command.startswith("A") or self.command.startswith("D")
                or self.command.startswith("M")):
            return "C_COMMAND"
        else:
            print("Invalid command ", self.command, "on line ", self.line)
        

    def symbol(self):
        if self.command.startswith("@"):
            try:
                return int(self.command[1:])
            except ValueError:
                if self.command.startswith("(") and self.command.find(")") > -1:
                    return self.command[0:self.command.find(")")]
                else:
                    print("Label command without a closing ')'", self.command)
                    return None
        

    def dest(self):
        if self.command.find("=") > -1:
            return self.command[0:self.command.find("=")]
        else:
            return None
        

    def comp(self):
        if self.command.find("=") > -1 and self.command.find(";") > -1:
            return self.command[self.command.find("=")+1:self.command.find(";")]
        elif self.command.find("=") > -1 and self.command.find(";") == -1:
            return self.command[self.command.find("=")+1:]
        elif self.command.find("=") == -1 and self.command.find(";") > -1:
            return self.command[0:self.command.find(";")]
        else:
            print("Invalid command ", self.command, ": computation without destination or jump")
            return None

    def jump(self):
        if self.command.find(";") > -1:
            return self.command[self.command.find(";")+1:]
        else:
            return None
        


class Code:
    def dest(self, dst):
        code = ""
        if dst.find("M") > -1:
            code += "1"
        else:
            code += "0"
        if dst.find("D") > -1:
            code += "1"
        else:
            code += "0"
        if dst.find("A") > -1:
            code += "1"
        else:
            code += "0"
        return code

    def comp(self, opr):
        code = ""
        if opr.find("M") > -1:
            op = opr.replace("M", "A")
            code += "1"
        else:
            op = opr
            code += "0"

        if op == "0":
            code += "101010"
        elif op == "1":
            code += "111111"
        elif op == "-1":
            code += "111010"
        elif op == "D":
            code += "001100"
        elif op == "A":
            code += "110000"
        elif op == "!D":
            code += "001101"
        elif op == "!A":
            code += "110001"
        elif op == "-D":
            code += "001111"
        elif op == "-A":
            code += "110011"
        elif op == "D+1":
            code += "011111"
        elif op == "A+1":
            code += "110111"
        elif op == "D-1":
            code += "011111"
        elif op == "A-1":
            code += "110111"
        elif op == "D+A":
            code += "000010"
        elif op == "D-A":
            code += "010011"
        elif op == "A-D":
            code += "000111"
        elif op == "D&A":
            code += "000000"
        elif op == "D|A":
            code += "010101"
        else:
            print("Invalid operation "+opr)
        return code

    def jump(self, jmp):
        if jmp == None:
            return "000"
        elif jmp == "JGT":
            return "001"
        elif jmp == "JEQ":
            return "010"
        elif jmp == "JGE":
            return "011"
        elif jmp == "JLT":
            return "100"
        elif jmp == "JNE":
            return "101"
        elif jmp == "JLE":
            return "110"
        elif jmp == "JMP":
            return "111"

SymbolTable = {}

def main(argv=sys.argv):
    c = Code()

    if len(argv) != 2:
        print("Usage: Assembler.py <file name>")
        return
    else:
        p = Parse(argv[1])
        
    #Pass 1
    address = 0
    while(p.advance()):
        if p.commandType() == "L_COMMAND":
            SymbolTable[p.symbol()] = address
        else:
            address += 1

    #Pass 2
    p = Parse(argv[1])
    address = 16
    while(p.advance()):
        code = ""
        print("line: "+str(p.line))
        print(p.command)
        print(p.commandType())
        if p.commandType() == "A_COMMAND":
            code += "0"
            print(p.symbol())
            if type(p.symbol()) is str:
                if SymbolTable.get(p.symbol(), None) is not None:
                    code += bin(SymbolTable[p.symbol()])[2:].zfill(15)
                else:
                    SymbolTable[p.symbol()] = address
                    code += bin(address)[2:].zfill(15)
                    address += 1
            else:
                code += bin(p.symbol())[2:].zfill(15)
        elif p.commandType() == "C_COMMAND":
            code += "111"
            print(p.comp())
            print(c.comp(p.comp()))
            code += c.comp(p.comp())
            print(p.dest())
            print(c.dest(p.dest()))
            code += c.dest(p.dest())
            print(p.jump())
            print(c.jump(p.jump()))
            code += c.jump(p.jump())
        elif p.commandType() == "L_COMMAND":
            print("Not implemented yet")
        print(code)

    

main()
