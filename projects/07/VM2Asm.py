import sys

class Parse:
    command = "\n"
    arg1 = None
    arg2 = None
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

        self.arg1 = None
        self.arg2 = None
        #print(self.command)
        if len(self.command.split()) ==1:
            pass
        elif len(self.command.split()) == 2:
            self.command, self.arg1 = self.command.split()
        elif len(self.command.split()) == 3:
            self.command, self.arg1, self.arg2 = self.command.split()
        else:
            print("Too many arguments to ", self.command.split()[0])
            

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
        

class CodeWriter:
    label_counter = 0
    class_name = ""

    def __init__(self, class_name):
        self.class_name = class_name

    def writePushPop(self, cmd, seg, indx):
        if cmd == "push":
            if seg == "constant":
                print("@"+indx+"\n"
                      "D=A\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            elif seg == "local":
                print("@LCL\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "A=A+D\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            elif seg == "argument":
                print("@ARG\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "A=A+D\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            elif seg == "this":
                print("@THIS\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "A=A+D\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            elif seg == "that":
                print("@THAT\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "A=A+D\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            elif seg == "pointer":
                print("@THIS\n"
                      "D=A\n"
                      "@"+indx+"\n"
                      "A=A+D\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")                
            elif seg == "temp":
                print("@5\n"
                      "D=A\n"
                      "@"+indx+"\n"
                      "A=A+D\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            elif seg == "static":
                print("@" + self.class_name + "." + indx + "\n"
                      "D=M\n"
                      "@SP\n"
                      "A=M\n"
                      "M=D\n"
                      "@SP\n"
                      "M=M+1\n")
            else:
                print("Unknown segment identifier" + seg + "\n")
        elif cmd == "pop":
            if seg == "constant":
                print("Error: cannot pop into 'constant' segment\n")
            elif seg == "local":
                print("@LCL\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "D=A+D\n"
                      "@R13\n"
                      "M=D\n"
                      "@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@R13\n"
                      "A=M\n"
                      "M=D\n")
            elif seg == "argument":
                print("@ARG\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "D=A+D\n"
                      "@R13\n"
                      "M=D\n"
                      "@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@R13\n"
                      "A=M\n"
                      "M=D\n")
            elif seg == "this":
                print("@THIS\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "D=A+D\n"
                      "@R13\n"
                      "M=D\n"
                      "@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@R13\n"
                      "A=M\n"
                      "M=D\n")
            elif seg == "that":
                print("@THAT\n"
                      "D=M\n"
                      "@"+indx+"\n"
                      "D=A+D\n"
                      "@R13\n"
                      "M=D\n"
                      "@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@R13\n"
                      "A=M\n"
                      "M=D\n")
            elif seg == "pointer":
                print("@THIS\n"
                      "D=A\n"
                      "@"+indx+"\n"
                      "D=A+D\n"
                      "@R13\n"
                      "M=D\n"
                      "@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@R13\n"
                      "A=M\n"
                      "M=D\n")
            elif seg == "temp":
                print("@5\n"
                      "D=A\n"
                      "@"+indx+"\n"
                      "D=A+D\n"
                      "@R13\n"
                      "M=D\n"
                      "@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@R13\n"
                      "A=M\n"
                      "M=D\n")
            elif seg == "static":
                print("@SP\n"
                      "AM=M-1\n"
                      "D=M\n"
                      "@" + self.class_name + "." + indx + "\n"
                      "M=D\n")
            else:
                      print("Unknown segment identifier " + seg + "\n")
        else:
            print("Internal error: writePushPop called with wrong command type\n")

    def writeArithmetic(self, cmd):
        #put the second arg in D
        print("@SP\n"
              "AM=M-1\n"
              "D=M\n")

        #if operator is binary
        #put the first arg in M
        if cmd not in ["neg", "not"]: #TODO: create unary operator list/function
            print("@SP\n"
                  "AM=M-1\n")

        #perform the specified operation,
        #leaving the result in D
        if cmd == "add":
            print("D=M+D\n")
        elif cmd == "sub":
            print("D=M-D\n")
        elif cmd == "neg":
            print("D=-D\n")
        elif cmd == "gt":
            self.label_counter += 1
            print("D=M-D\n"
                  "@GT" + str(self.label_counter) + "\n"
                  "D;JGT\n"
                  "D=0\n"
                  "@GT_END" + str(self.label_counter) + "\n"
                  "0;JMP\n" 
                  "(GT" + str(self.label_counter) + ")\n"
                  "D=-1\n"
                  "(GT_END" + str(self.label_counter) + ")\n")
        elif cmd == "lt":
            self.label_counter += 1
            print("D=M-D\n"
                  "@LT" + str(self.label_counter) + "\n"
                  "D;JLT\n"
                  "D=0\n"
                  "@LT_END" + str(self.label_counter) + "\n"
                  "0;JMP\n" 
                  "(LT" + str(self.label_counter) + ")\n"
                  "D=-1\n"
                  "(LT_END" + str(self.label_counter) + ")\n")
        elif cmd == "eq":
            self.label_counter += 1
            print("D=M-D\n"
                  "@EQ" + str(self.label_counter) + "\n"
                  "D;JEQ\n"
                  "D=0\n"
                  "@EQ_END" + str(self.label_counter) + "\n"
                  "0;JMP\n" 
                  "(EQ" + str(self.label_counter) + ")\n"
                  "D=-1\n"
                  "(EQ_END" + str(self.label_counter) + ")\n")
        elif cmd == "and":
            print("D=D&M\n")
        elif cmd == "or":
            print("D=D|M\n")
        elif cmd == "not":
            print("D=!D\n")

        #push the result on the stack
        print("@SP\n"
              "A=M\n"
              "M=D\n"
              "@SP\n"
              "M=M+1\n")
        
def main(argv=sys.argv):

    if len(argv) != 2:
        print("Usage: VM2Asm.py <file name>")
        return
    else:
        p = Parse(argv[1])

    c = CodeWriter(argv[1].rpartition('/')[-1].rpartition('.')[0])
        
    while(p.advance()):
        print("// " + p.command + " " + str(p.arg1) + " " + str(p.arg2))
        if p.commandType() == "C_ARITHMETIC":
            c.writeArithmetic(p.command)
        elif p.commandType() in ["C_PUSH", "C_POP"]:
            if p.arg1 and p.arg2:
                c.writePushPop(p.command, p.arg1, p.arg2)
            else:
                print("Missing arguments to ", p.command)
        else:
            print("Unsupported command\n")

main()
