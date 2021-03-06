#Compilador L2C-bot
#Main
#Arturo Rojas Ortiz     A01039185
#Diego Jimenez Torres   A01139513

import ply.lex as lex
import ply.yacc as yacc
import re
import codecs
import os
import sys


compileFlag = True

curScope = 'global'
funcTable = {}
tempVars = {}
tempVars = {'sub' : {}} 
tempType = ''
cont = 0

sem_cube = {'int' : 	{ 'int' : { '+': 'int',
                                    '-': 'int',
                                    '/': 'float',
                                    '*': 'int',
                                    '%': 'int',
                                    '<': 'bool',
                                    '>': 'bool',
                                    '<=': 'bool',
                                    '>=': 'bool',
                                    '!=': 'bool',
                                    '==': 'bool',
                                    '=': 'int'},
                          'float': {'+': 'float',
                                    '-': 'float',
                                    '/': 'float',
                                    '*': 'float',
                                    '%': 'float',
                                    '<': 'bool',
                                    '>': 'bool',
                                    '<=': 'bool',
                                    '>=': 'bool',
                                    '!=': 'bool',
                                    '==': 'bool',
                                    '=': 'int'}},
                 'float' : {'int' : {'+': 'float',
                                    '-': 'float',
                                    '/': 'float',
                                    '*': 'float',
                                    '%': 'float',
                                    '<': 'bool',
                                    '>': 'bool',
                                    '<=': 'bool',
                                    '>=': 'bool',
                                    '!=': 'bool',
                                    '==': 'bool',
                                     '=': 'float'},
                          'float': {'+': 'float',
                                    '-': 'float',
                                    '/': 'float',
                                    '*': 'float',
                                    '%': 'float',
                                    '<': 'bool',
                                    '>': 'bool',
                                    '<=': 'bool',
                                    '>=': 'bool',
                                    '!=': 'bool',
                                    '==': 'bool',
                                    '=': 'float'}},
                 'bool' : {'bool' : {'and': 'bool',
                                     'or': 'bool',
                                     '=' : 'bool'}},
                'string' : {'string': {'+': 'string',
                                       '=': 'string',
                                       '==': 'bool',
                                       '!=': 'bool'},
                            'char':{'+': 'string',
                                    '=': 'string',
                                    '==': 'bool',
                                    '!=': 'bool'}},
                'char' : {'char' : {'+':'stirng',
                                    '=': 'char',
                                    '==': 'bool',
                                    '!=': 'bool'},
                            'string':{'+': 'string',
                                      '=':'string',
                                      '==': 'bool',
                                      '!=': 'bool'}}}



reserved = {
    'program'   : 'PROGRAM',
    'main'      : 'MAIN',
    'vardef'    : 'VARDEF',
    'funcdef'   : 'FUNCDEF', 
    'call'      : 'CALL',
    'if'        : 'IF',
    'else'      : 'ELSE',
    'cin'       : 'CIN',
    'cout'      : 'COUT',
    'delay'     : 'DELAY',
    'forward'   : 'FORWARD',
    'backward'  : 'BACKWARD',
    'turnleft'  : 'TURNLEFT',
    'turnright' : 'TURNRIGHT',
    'servo'     : 'SERVO',
    'lights'    : 'LIGHTS',
    'display'   : 'DISPLAY',
    'distance'  : 'DISTANCE',
    'stop'      : 'STOP',
    'return'    : 'RETURN',
    'fin'       : 'FIN',
    'int'       : 'INT',
    'float'     : 'FLOAT',
    'char'      : 'CHAR',
    'bool'      : 'bool',
    'void'      : 'VOID',
    'string'    : 'STRING'
}

tokens = ['ASSIGN', 'PLUS', 'MINUS', 'MULTI', 'DIVI', 'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LCURLY', 'RCURLY', 'EQUALS', 'LESSTHAN', 'GREATERTHAN', 'NOTEQUAL', 'SEMICOLON', 'COMMA', 'AND', 'OR', 'CTE_STRING', 'CTE_INT', 'CTE_FLOAT', 'CTE_CHAR', 'CTE_ARR', 'ID']  + list(reserved.values())
t_ASSIGN    = r'='
t_PLUS      = r'\+'
t_MINUS     = r'-'
t_MULTI     = r'\*'
t_DIVI      = r'\/'
t_LPAREN    = r'\('
t_RPAREN    = r'\)'
t_LBRACKET  = r'\['
t_RBRACKET  = r'\]'
t_LCURLY    = r'\{'
t_RCURLY    = r'\}'
t_EQUALS    = r'=='
t_LESSTHAN  = r'<'
t_GREATERTHAN = r'>'
t_NOTEQUAL  = r'<>'
t_SEMICOLON = r';'
t_COMMA     = r','
t_AND       = r'\&\&'
t_OR        = r'\|\|'
t_CTE_INT   = r'[0-9]+'
t_CTE_CHAR  = r'\'[a-zA-Z0-9]\''
t_CTE_STRING = r'\"[a-zA-Z0-9]+\"'
t_CTE_FLOAT = r'[0-9]+\.[0-9]+'

def t_error(t):
    global compileFlag
    compileFlag = False
    print("Caracter ilegal '%s'" % t.value[0])
    t.lexer.skip(1)

def t_CTE_ARR(t):
    r'\[[0-9]\]'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value,'ID')
    return t

t_ignore = " \t"

def t_COMMENT(t):
    r'\#.*'
    pass

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

lexer = lex.lex()

def p_program(p):
    'program : PROGRAM globalvarsblock funcsblock main FIN SEMICOLON'

def p_main(p):
    'main : MAIN LPAREN RPAREN LCURLY main2'
    global curScope
    global tempVars
    curScope = 'main'
    print(curScope, "in main")
    funcTable[curScope] = {'type' : 'void', 'sub' : {}}
    funcTable[curScope]['sub']= tempVars
    tempVars = {'sub' : {}} 
    

def p_main2(p):
    'main2 : varsblock block RCURLY'
    print(curScope, "in main")
    

def p_funcsblock(p):
    '''funcsblock : funcs funcsblock 
                  | empty'''
    

def p_funcs(p):
    'funcs : FUNCDEF choosetype ID LPAREN params RPAREN LCURLY varsblock block RCURLY'''
    global curScope
    global tempVars
    curScope = p[3]
    funcTable[curScope] = {'type' : 'void', 'sub' : {}}
    funcTable[curScope]['sub']= tempVars
    tempVars = {'sub' : {}} 

def p_globalvarsblock(p):
    '''globalvarsblock : vars varsblock 
                 | empty'''
    global curScope
    global tempVars
    funcTable[curScope] = {'type' : 'void', 'sub' : {}}
    funcTable[curScope]['sub']= tempVars
    tempVars = {'sub' : {}} 

def p_varsblock(p):
    '''varsblock : vars varsblock 
                 | empty'''

def p_vars(p):
    'vars : VARDEF type ID vars1 SEMICOLON'
    global tempVars
    global cont
    cont = cont +1
    print("im in var", cont)
    tempVars['sub'][p[3]] = {'type' : tempType}
    print(tempVars)

def p_vars1(p):
    '''vars1 : LBRACKET CTE_INT RBRACKET 
             | empty'''

def p_choosetype(p):
    '''choosetype : type
                  | VOID'''

def p_params(p):
    '''params : type ID params1
              | empty'''

def p_params1(p): 
    '''params1 : COMMA type ID params1 
               | empty'''

def p_block(p):
    '''block : statute block
             | empty'''

def p_statute(p):
    '''statute : cond
               | assign
               | call 
               | cin
               | cout
               | delay
               | forward
               | backward
               | turnleft
               | turnright
               | servo
               | lights
               | display
               | distance
               | stop
               | return'''

def p_cond(p):
    'cond : IF LPAREN express RPAREN block else SEMICOLON'

def p_else(p):
    '''else : ELSE block 
            | empty'''

def p_assign(p):
    '''assign :  ID assign1 ASSIGN express SEMICOLON'''

def p_assign1(p):
    '''assign1 : LBRACKET express RBRACKET 
               | empty'''

def p_call(p):
    'call : CALL ID LPAREN call1 RPAREN SEMICOLON'
    
def p_call1(p):
    '''call1 : express call2 
             | empty'''

def p_call2(p):
    '''call2 : COMMA call1 
             | empty'''

def p_cin(p):
    'cin : CIN cin1 SEMICOLON'

def p_cin1(p):
    '''cin1 : cin2 
            | cin3'''

def p_cin2(p):
    'cin2 : LPAREN ID RPAREN'

def p_cin3(p):
    'cin3 : LBRACKET CTE_INT RBRACKET LPAREN cin4 RPAREN'

def p_cin4(p):
    '''cin4 : COMMA ID cin4 
           | empty'''

def p_cout(p):
    'cout : COUT LPAREN express RPAREN SEMICOLON'

def p_delay(p):
    'delay : DELAY LPAREN CTE_INT RPAREN SEMICOLON'

def p_forward(p):
    'forward : FORWARD LPAREN express COMMA express RPAREN SEMICOLON'

def p_backward(p):
    'backward : BACKWARD LPAREN express COMMA express RPAREN SEMICOLON'

def p_turnleft(p):
    'turnleft : TURNLEFT LPAREN express COMMA express RPAREN SEMICOLON'

def p_turnright(p):
    'turnright : TURNRIGHT LPAREN express COMMA express RPAREN SEMICOLON'

def p_servo(p):
    'servo : SERVO LPAREN express RPAREN SEMICOLON'

def p_lights(p):
    'lights :  LIGHTS LPAREN CTE_INT COMMA CTE_INT RPAREN SEMICOLON'

def p_display(p):
    'display : DISPLAY LPAREN CTE_CHAR RPAREN SEMICOLON'

def p_distance(p):
    'distance : DISTANCE LPAREN RPAREN SEMICOLON'

def p_stop(p):
    'stop : STOP LPAREN RPAREN SEMICOLON'

def p_return(p):
    'return : RETURN express SEMICOLON'

def p_type(p):
    '''type : INT
            | FLOAT
            | bool
            | CHAR
            | STRING'''
    global tempType
    tempType = p[1]
        
def p_constant(p):
    '''constant : ID
                | CTE_INT
                | CTE_FLOAT
                | CTE_STRING
                | CTE_CHAR'''

def p_express(p):
    'express : compare express1'

def p_express1(p):
    '''express1 : express2 compare
                | empty'''

def p_express2(p):
    '''express2 : AND
                | OR'''

def p_compare(p):
    '''compare : exp compare1'''

def p_compare1(p):
    '''compare1  : LESSTHAN exp
                 | GREATERTHAN exp
                 | EQUALS exp
                 | NOTEQUAL exp
                 | empty'''

def p_exp(p):
    'exp : term exp1'

def p_exp1(p):
    '''exp1 : exp2 term 
            | empty'''

def p_exp2(p):
    '''exp2 : PLUS 
            | MINUS'''

def p_term(p):
    'term :  factor term1'

def p_term1(p):
    '''term1 : term2 term 
             | empty'''

def p_term2(p):
    '''term2 : MULTI 
             | DIVI'''

def p_factor(p): 
    '''factor : LPAREN express RPAREN
              | factor1 constant'''
            
def p_factor1(p):
    '''factor1 : PLUS
               | MINUS
               | empty'''

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print ("Illegal tokennn", p)
    global compileFlag 
    compileFlag = False
    ex = p.lexer.lexstateinfo
    print(ex)

parser = yacc.yacc()

filename = "docs/tests/prueba1.txt"
fp = codecs.open(filename,"r")
nextline = fp.read()
fp.close()
lexer.input(nextline)
parser.parse(nextline)


if compileFlag == True:
    print("Compila!!!")
    for x in funcTable.items():
        print(x)

else:
    print("No compila")


    