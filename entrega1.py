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
    'fin'       : 'FIN'
}

tokens = ['ASSIGN', 'PLUS', 'MINUS', 'MULTI', 'DIVI', 'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LCURLY', 'RCURLY', 'EQUALS', 'LESSTHAN', 'GREATERTHAN', 'NOTEQUAL', 'SEMICOLON', 'COMMA', 'AND', 'OR', 'CTE_INT', 'CTE_FLOAT', 'CTE_CHAR', 'ID']  + list(reserved.values())
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
t_CTE_CHAR  = r'\'[a-zA-Z0-9 ]\''

def t_CTE_INT(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_CTE_FLOAT(t):
    r'[0-9]+\.[0-9]*'
    t.value = float(t.value)
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

def t_error(t):
    print ("Illegal token '%s'" % t.value[0])
    global compileFlag 
    compileFlag = False
    t.lexer.skip(1)

lexer = lex.lex()

def p_program(p):
    'program : PROGRAM program1 program2 main FIN SEMICOLON'

def p_program1(p):
    '''program1 : varsblock 
                | empty'''

def p_program2(p):
    '''program2 : funcsblock 
                | empty'''

def p_main(p):
    'main : FUNCDEF MAIN LPAREN RPAREN LCURLY main1 main2 RCURLY'

def p_main1(p):
    '''main1 : varsblock 
             | empty'''

def p_main2(p):
    '''main2 : block 
             | empty'''

def p_varsblock(p):
    '''varsblock : vars varsblock 
                 | empty'''

def p_vars(p):
    'vars : VARDEF type vars1 SEMICOLON'

def p_vars1(p):
    '''vars1 : vars2 
             | vars3'''

def p_vars2(p):
    'vars2 : LBRACKET CTE_INT RBRACKET ID ASSIGN ARR vars4'

def p_vars3(p):
    'vars3 : ID ASSIGN CTE_VAR vars5'

def p_vars4(p):
    '''vars4 : ARR COMMA vars4 
             | empty'''

def p_vars5(p):
    '''vars5 : CTE_VAR COMMA vars5 
             | empty'''

def p_funcsblock(p):
    '''funcsblock : funcs funcsblock 
                  | empty'''

def p_funcs(p):
    'funcs : FUNCDEF type ID LPAREN funcs1 RPAREN LCURLY funcs2 funcs3 RCURLY SEMICOLON'

def p_funcs1(p):
    '''funcs1 : params 
              | empty'''

def p_funcs2(p):
    '''funcs2 : varsblock 
              | empty'''

def p_funcs3(p):
    '''funcs3 : statute 
              | empty'''

def p_params(p):
    'params : type ID params1'

def p_params1(p): 
    '''params1 : COMMA params 
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
    'assign :  ID assign1 ASSIGN express SEMICOLON'

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

def p_p_distance(p):
    'distance : DISTANCE LPAREN RPAREN SEMICOLON'

def p_stop(p):
    'stop : STOP LPAREN RPAREN SEMICOLON'

def p_return(p):
    'return : RETURN express SEMICOLON'

def p_block(p):
    '''block : statute block 
             | empty'''

def p_type(p):
    '''type : INT
            | FLOAT
            | BOOL
            | CHAR'''
        
def p_CTE_VAR(p):
    '''CTE_VAR : ID
               | CTE_INT
               | CTE_FLOAT
               | CTE_STRING
               | CTE_BOOL'''

def p_condition(p):
    'condition : express condition1'

def p_condition1(p):
    '''condition1 : andor express 
                  | empty'''

def p_andor(p):
    '''andor : AND
             | OR'''

def p_express(p):
    'express : exp express1'

def p_express1(p):
    '''express1 : LESSTHAN exp
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
    '''factor : express 
              | factor1'''

def p_factor1(p):
    'factor1 : factor2 CTE_VAR'

def p_factor2(p):
    '''factor2 : PLUS 
               | MINUS 
               | empty'''

def p_empty(p):
    'empty :'
    pass

parser = yacc.yacc()

filename = "docs/tests/prueba1.txt"
fp = codecs.open(filename,"r")
nextline = fp.read()
fp.close()
lexer.input(nextline)

parser.parse(nextline)

while True:
    tok = lexer.token()
    if not tok : break
    print(tok)

if compileFlag == False: 
    print("No compila")


    