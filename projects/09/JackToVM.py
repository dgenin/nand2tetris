import sys

keyword = ['class', 'constructor', 'function', 'method', 'field', 'static', 'var', 'int', 'char', 'boolean', 'void', 'true', 'false', 'null', 'this', 'let', 'do', 'if', 'else', 'while', 'return']

symbol = ['{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~']

class JackTokenizer:
    symbol = None
    next_symbol = None
    token = None
    token_type = None
    position = 0
    source = None

    def __init__(source):
        self.source = source
        symbol = source[position]
        next_symbol = source[position + 1]

    def hasMoreTokens():
        return position <= length(source)

    def advance():
        if self.source[position].isspace():
            while self.source[position].isspace():
                position += 1

        if self.source[position].isalpha():
            self.token = self._readWord()
            if self.token in keyword:
                self.token_type = 'KEYWORD'
            else:
                self.token_type = 'IDENTIFIER'
        elif self.source[position] in symbol:
            self.token = self.soruce[position]
            self.token_type = 'SYMBOL'
            self.position += 1
        elif self.source[position].isdigit():
            self.token = readInteger()
            self.token_type = 'INTEGER'
        elif self.source[position] == '"':
            self.readString()
        else:
            print("Unknown expression at position %d" % position)

    def _readWord():
        start = self.position
        while self.source[position].isalpha() or self.source[position].isdigit():
            position += 1
        return self.source[start:position]

    def _readInteger():
        start = self.position
        while self.source[position].isdigit():
            self.position += 1
        return int(self.source[start:position])

    def _readString():
        start = self.position
        while self.source[position] != '"':
            self.position += 1
        end = self.position
        self.position += 1
        return self.source[start:end]

    def tokenType():
        return token_type

    def keyWord():
        return token

    def symbol():
        return token

    def identifier():
        return token

    def intVal():
        return token

    def stringVal():
        return token
        

def main():
    with open(sys.argv[1]) as f:
        tokenizer = JackTokenizer(f.read())
    while tokenizer.hasMoreTokens():
        tokenizer.advance()
        print(tokenizer.tokenType + " ")
        print(str(tokenizer.token) + "\n")

main()
