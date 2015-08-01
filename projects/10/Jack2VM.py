import sys

keyword = ['class', 'constructor', 'function', 'method', 'field', 'static', 'var', 'int', 'char', 'boolean', 'void', 'true', 'false', 'null', 'this', 'let', 'do', 'if', 'else', 'while', 'return']

types = ['int', 'char', 'boolean']

subroutine = ['constructor', 'function', 'method']

symbol = ['{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~']

class JackTokenizer:
    token = None
    token_type = None
    position = 0
    source = None

    def __init__(self, source):
        self.source = source
        self.position = 0

    def hasMoreTokens(self):
        return self.position < len(self.source)

    def _skip_whitespace(self):
        if self.source[self.position].isspace():
            while self.source[self.position].isspace():
                self.position += 1

    def _skip_comments(self):
        self._skip_whitespace()
        while (self.source[self.position:self.position+2] == '//' or
               self.source[self.position:self.position+3] == '/**'):
            if self.source[self.position:self.position+2] == '//':
                while self.source[self.position] != '\n':
                    self.position += 1
            if self.source[self.position:self.position+3] == '/**':
                while self.source[self.position:self.position+2] != '*/':
                    self.position += 1
                self.position += 2
            self._skip_whitespace()
        self._skip_whitespace()

    def advance(self):
        try:
            self._skip_comments()
        except IndexError:
            return
        
        if self.source[self.position].isalpha():
            self.token = self._readWord()
            if self.token in keyword:
                self.token_type = 'KEYWORD'
            else:
                self.token_type = 'IDENTIFIER'
        elif self.source[self.position] in symbol:
            self.token = self._readSymbol()
            self.token_type = 'SYMBOL'
        elif self.source[self.position].isdigit():
            self.token = self._readInteger()
            self.token_type = 'INTEGER'
        elif self.source[self.position] == '"':
            self._readString()
        else:
            print("Unknown expression at position %d" % self.position)
            sys.exit(1)

    def _readWord(self):
        start = self.position
        while self.source[self.position].isalpha() or self.source[self.position].isdigit():
            self.position += 1
        return self.source[start:self.position]

    def _readInteger(self):
        start = self.position
        while self.source[self.position].isdigit():
            self.position += 1
        return int(self.source[start:self.position])

    def _readString(self):
        start = self.position
        while self.source[self.position] != '"':
            self.position += 1
        end = self.position
        self.position += 1
        return self.source[start:end]

    def _readSymbol(self):
        pos = self.position
        self.position += 1
        return self.source[pos]

    def tokenType(self):
        return self.token_type

    def keyWord(self):
        return self.token

    def symbol(self):
        return self.token

    def identifier(self):
        return self.token

    def intVal():
        return self.token

    def stringVal():
        return self.token


class CompilationEngine:
    tokenizer = None
    output = ''
    indent = 0
    
    def __init__(self, tokenizer):
        self.tokenizer = tokenizer
        self.tokenizer.advance()
        sys.stdout.write(self._prettyPrint('class', self.compileClass()))

    def _prettyPrint(self, tag, value):
        sys.stderr.write('_prettyPrint('+tag+','+value+')\n')
        return '\t'*self.indent + '<' + tag + '> ' + value + ' </ ' + tag + '>'

    def compileClassVarDec(self):
        return self._prettyPrint('classVarDec', '')

    def _parseParameterList(self):
        ret = ''
        self.indent += 1

        self.tokenizer.advance()
        while self.tokenizer.tokenType() == 'KEYWORD' and self.tokenizer.keyWord() in types:
            ret += self._prettyPrint('keyword', self.tokenizer.keyWord())

            self.tokenizer.advance()
            if self.tokenizer.tokenType() == 'IDENTIFIER':
                ret += self._prettyPrint('identifier', self.tokenizer.identifier())
            else:
                sys.stderr.write('Expected identifier but got ' + self.tokenizer.tokenType() + ' following type in parameter list')
                sys.exit(1)

            self.tokenizer.advance()
            if self.tokenizer.tokenType() == 'SYMBOL':
                if self.tokenizer.symbol() == ',':
                    ret += self._prettyPrint('symbol', self.tokenizer.symbol())
                elif self.tokenizer.symbol() == ')':
                    break

            self.tokenizer.advance()

        if self.tokenizer.tokenType() == 'SYMBOL' and self.tokenizer.symbol() == ')':
            return ret
        else:
            sys.stderr.write('Unexpected token ' + self.tokenizer.tokenType() + ' in parameter list')
            sys.exit(1)
        

    def compileSubroutineDec(self):
        subroutine_dec = ''

        self.indent += 1
        self._prettyPrint('keyword', self.tokenizer.keyWord())
        subroutine_type = self.tokenizer.keyWord()

        self.tokenizer.advance()
        if self.tokenizer.tokenType() == 'KEYWORD' and self.tokenizer.keyWord() in (types + ['void']):
            subroutine_dec += self._prettyPrint('keyword', self.tokenizer.keyWord())
        elif self.tokenizer.tokenType() == 'IDENTIFIER' and self.tokenizer.identifier() in types:
            subroutine_dec += self._prettyPrint('identifier', self.tokenizer.identifier())
        else:
            sys.stderr.write('Unexpected token ' + self.tokenizer.token + ' in ' + subroutine_type + ' declaration\n')

        self.tokenizer.advance()
        if self.tokenizer.tokenType() == 'IDENTIFIER':
            subroutine_dec += self._prettyPrint('identifier', self.tokenizer.identifier())
        else:
            sys.stderr.write('Unexpected token following ' + subroutine_type + ' keyword\n')
            sys.exit(1)

        self.tokenizer.advance()
        if self.tokenizer.tokenType() == 'SYMBOL' and self.tokenizer.symbol() == '(':
            subroutine_dec += self._prettyPrint('symbol', self.tokenizer.symbol())
            subroutine_dec += self._prettyPrint('parameterList', self._parseParameterList())
            subroutine_dec += self._prettyPrint('symbol', self.tokenizer.symbol())
        else:
            sys.stderr.write('Unexpected token ' + self.tokenizer.token + ' in ' + subroutine_type + ' declaration\n')
            sys.exit(1)

        self.tokenizer.advance()
            
        return subroutine_dec

    def compileClass(self):
        global types
        global subroutine
        if self.tokenizer.keyWord() == 'class':
            self.indent += 1
            self._prettyPrint('keyword', self.tokenizer.keyWord())
            self.tokenizer.advance()
            if self.tokenizer.tokenType() == 'IDENTIFIER':
                class_name = self.tokenizer.identifier()
                types = types + [class_name]
                self._prettyPrint('identifier', self.tokenizer.identifier())
                self.tokenizer.advance()
                if self.tokenizer.tokenType() == 'SYMBOL' and self.tokenizer.symbol() == '{':
                    self._prettyPrint('symbol', self.tokenizer.symbol())
                    self.tokenizer.advance()
                    if self.tokenizer.tokenType() == 'KEYWORD':
                        while self.tokenizer.tokenType() == 'KEYWORD':
                            if self.tokenizer.keyWord() == 'var':
                                self._prettyPrint('classVarDec', self.compileClassVarDec())
                            elif self.tokenizer.keyWord() in subroutine:
                                self._prettyPrint('subroutineDec', self.compileSubroutineDec())
                            else:
                                sys.stderr.write('Unexpected keyword ' + self.tokenizer.keyWord() + ' in class ' + class_name)
                                sys.exit(1)
                            self.tokenizer.advance()
                    else:
                        sys.stderr.write('Expected keyword but got ' + self.tokenType() + ' in class ' + class_name)
                else:
                    sys.stderr.write('Expected class name identifier but got ' + self.tokenizer.tokenType)
            else:
                sys.stderr.write('Expected keyword "class" but got ' + self.tokenizer.tokenType())


def main():
    with open(sys.argv[1]) as f:
        tokenizer = JackTokenizer(f.read())

    CompilationEngine(tokenizer)
    # while tokenizer.hasMoreTokens():
    #     tokenizer.advance()
    #     sys.stdout.write('<' + tokenizer.tokenType().lower() + '>')
    #     sys.stdout.write(str(tokenizer.token))
    #     sys.stdout.write('<' + tokenizer.tokenType().lower() + '/>\n')
    # pass

main()
