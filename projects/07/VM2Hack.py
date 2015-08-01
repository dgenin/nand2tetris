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
        if self.command.partition(" ")[0] in ['add', 'sub', 'neg', 'eq', 'gt', 'lt', 'and', 'or', 'not']:
            return "C_ARITHMETIC"
        elif self.command.startswith("push"):
            return "C_PUSH"
        elif self.command.startswith("pop"):
            return "C_POP"
        elif self.command.startswith("label"):
            return "C_LABEL"
        elif self.command.startswith("goto"):
            return "C_GOTO"
        elif self.command.startswith("function"):
            return "C_FUNCTION"
        elif self.command.startswith("return"):
            return "C_RETURN"
        elif self.command.startswith("call"):
            return "C_CALL"
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

class CodeWriter:
    label_counter = 0

    def writePushPop(self, cmd, seg, indx):
        if cmd == "C_PUSH":
            if seg == "constant":
                print("@ "+str(indx)+"\n"
                      "D=A\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
        if cmd == "C_POP":
            if seg == "constant":
                print("Error: cannot pop into 'constant' segment")

    def writeArithmetic(self, cmd):
        #put the first arg in D
        print("@SP\n"
              "AM=A-1\n"
              "D=M\n")
        #put the second arg in M
        print("@SP\n"
              "AM=A-1\n")
        if cmd == "add":
            print("D=D+M\n")
        elif cmd == "sub":
            print("D=D-M\n")
        elif cmd == "neg":
            print("D=-D\n")
        elif cmd == "gt":
            label_counter += 1
            print("@GT" + str(label_counter) + "\n"
                  "D=D-M;JGT\n"
                  "@0\n"
                  "D=A\n"
                  "@GT_END" + str(label_counter) + "\n"
                  "JMP\n" 
                  "(GT" + str(label_counter) + ")\n"
                  "@FFFF\n"
                  "D=A\n"
                  "(GT_END" + str(label_counter) + ")\n")
        elif cmd == "lt":
            label_counter += 1
            print("@LT" + str(label_counter) + "\n"
                  "D=D-M;JLT\n"
                  "@0\n"
                  "D=A\n"
                  "@LT_END" + str(label_counter) + "\n"
                  "JMP\n" 
                  "(LT" + str(label_counter) + ")\n"
                  "@FFFF\n"
                  "D=A\n"
                  "(LT_END" + str(label_counter) + ")\n")
        elif cmd == "eq":
            label_counter += 1
            print("@EQ" + str(label_counter) + "\n"
                  "D=D-M;JEQ\n"
                  "@0\n"
                  "D=A\n"
                  "@EQ_END" + str(label_counter) + "\n"
                  "JMP\n" 
                  "(EQ" + str(label_counter) + ")\n"
                  "@FFFF\n"
                  "D=A\n"
                  "(EQ_END" + str(label_counter) + ")\n")
        elif cmd == "and":
            print("D=D&M\n")
        elif cmd == "or":
            print("D=D|M\n")
        elif cmd == "not":
            print("D=!D\n")

        
def main(argv=sys.argv):
    c = CodeWriter()

    if len(argv) != 2:
        print("Usage: VM2Asm.py <file name>")
        return
    else:
        p = Parse(argv[1])
        
    while(p.advance()):
        if p.commandType() == "C_ARITHMETIC":
            CodeWriter.writeArithmetic(p.command)
        elif p.commandType() in ["C_PUSH", "C_POP"]:
            CodeWriter.writePushPop(p.command)
        else:
            print("Unsupported command\n")

main()
