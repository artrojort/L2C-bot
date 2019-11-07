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

#operadores
popers = []
#variables
pconsts = []
#tipo
ptypes = []
#saltos para GOTOS
pjumps = []
#cuadruplos
quads = []

#temporales
tempindex = 0

iQuads = 0
scope = 'global'
funcTable = {}
tempVars = {'varsTable' : {}} 
tempParams = {'params' : {}}
tempType = ''
cont = 0

opeCode = {
	'GOTO'      : 0,
    'GOTOF'     : 1,
    'GOTOV'     : 2,
    '='         : 3,
    '<'         : 4,
    '>'         : 5,
    '<>'        : 6,
	'+'         : 7,
	'-'         : 8,
    '*'         : 9,
    '/'         : 10,
    'cout'      : 11
}

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False

def newTemp(varType):
    global tempindex
    x = "~t" + str(tempindex)
    tempindex = tempindex + 1
    pconsts.append(x)
    ptypes.append(varType)
    return x

semCube = {'int' : { 'int' : {  '+' : 'int',
                                '-' : 'int',
                                '/' : 'float',
                                '*' : 'int',
                                '<' : 'bool',
                                '>' : 'bool',
                                '==': 'bool',
                                '!=': 'bool',
                                '=' : 'int'
                            },
                    'float' : { '+' : 'float',
                                '-' : 'float',
                                '/' : 'float',
                                '*' : 'float',
                                '<' : 'bool',
                                '>' : 'bool',
                                '==': 'bool',
                                '!=': 'bool',
                                '=' : 'int'
                            }
                    },
            'float' :{ 'int' : {'+' : 'float',
                                '-' : 'float',
                                '/' : 'float',
                                '*' : 'float',
                                '<' : 'bool',
                                '>' : 'bool',
                                '==': 'bool',
                                '!=': 'bool',
                                '=' : 'float'
                                },
                       'float':{'+': 'float',
                                '-': 'float',
                                '/': 'float',
                                '*': 'float',
                                '<': 'bool',
                                '>': 'bool',
                                '==': 'bool',
                                '!=': 'bool',
                                '=': 'float'
                                }
                    },
            'bool' : {'bool' : {'&&': 'bool',
                                '||': 'bool',
                                '=' : 'bool',
                                '==': 'bool',
                                '!=': 'bool'
                                }
                    },
            'char' : {'char' : { '==' : 'bool',
                                 '!=' : 'bool'
                                 }
                    }
        }


def typeCheck(ope, a, b):
    try:
        x = semCube[a][b][ope]
        return x
    except KeyError:
        # Key is not present
        errormsg = "ERROR: " + a + ope +  b + " is not a valid operation."
        print(errormsg)
        #SYS EXIT DISABLED DURING DEBUGGING
        #sys.exit(errormsg)
        return False

#EJEMPLO
print(typeCheck('+', 'int', 'char'))

def newQuad(ope, a, b, res):
    global iQuads
    quads.append([ope, a, b, res])
    iQuads = iQuads + 1

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
    'while'     : 'WHILE',
    'return'    : 'RETURN',
    'fin'       : 'FIN',
    'int'       : 'INT',
    'float'     : 'FLOAT',
    'char'      : 'CHAR',
    'bool'      : 'BOOL',
    'void'      : 'VOID'
}

tokens = ['ASSIGN', 'PLUS', 'MINUS', 'MULTI', 'DIVI', 'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LCURLY', 'RCURLY', 'EQUALS', 'LESSTHAN', 'GREATERTHAN', 'NOTEQUALS', 'SEMICOLON', 'COMMA', 'AND', 'OR', 'NOT', 'CTE_BOOL', 'CTE_INT', 'CTE_FLOAT', 'CTE_CHAR', 'CTE_ARR', 'ID']  + list(reserved.values())
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
t_NOTEQUALS = r'<>'
t_LESSTHAN  = r'<'
t_GREATERTHAN = r'>'
t_SEMICOLON = r';'
t_COMMA     = r','
t_AND       = r'\&\&'
t_OR        = r'\|\|'
t_NOT       = r'!='
t_CTE_INT   = r'[0-9]+'
t_CTE_CHAR  = r'\'[a-zA-Z0-9]\''
t_CTE_FLOAT = r'[0-9]+\.[0-9]+'
t_CTE_BOOL  = r'[true|false]'

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
    global scope
    global tempVars
    scope = 'main'
    funcTable[scope] = {'type' : 'void', 'params' : {}, 'varsTable' : {}}
    funcTable[scope]['varsTable']= tempVars['varsTable']
    funcTable[scope]['params']= tempParams['params']
    tempVars = {} 
    

def p_main2(p):
    'main2 : varsblock block RCURLY'
    

def p_funcsblock(p):
    '''funcsblock : funcs funcsblock 
                  | empty'''
    

def p_funcs(p):
    'funcs : FUNCDEF choosetype ID LPAREN paramsblock RPAREN LCURLY varsblock block RCURLY'''
    global scope
    global tempVars
    global tempParams
    scope = p[3]
    funcTable[scope] = {'type' : tempType, 'params' : {}, 'varsTable' : {}}
    funcTable[scope]['varsTable'] = tempVars['varsTable']
    funcTable[scope]['params'] = tempParams['params']
    tempVars = {'varsTable' : {}} 
    tempParams = {'params' : {}}

def p_globalvarsblock(p):
    '''globalvarsblock : vars varsblock 
                       | empty'''
    global scope
    global tempVars
    global tempParams
    funcTable[scope] = {'type' : 'void', 'params' : {}, 'varsTable' : {}}
    funcTable[scope]['varsTable'] = tempVars['varsTable']
    funcTable[scope]['params'] = tempParams['params']
    tempVars = {'varsTable' : {}} 
    tempParams = {'params' : {}} 

def p_varsblock(p):
    '''varsblock : vars varsblock 
                 | empty'''

def p_vars(p):
    'vars : VARDEF type ID vars1 SEMICOLON'
    global tempVars
    tempVars['varsTable'][p[3]] = {'type' : tempType}

def p_vars1(p):
    '''vars1 : LBRACKET CTE_INT RBRACKET 
             | empty'''

def p_choosetype(p):
    '''choosetype : type
                  | VOID'''

def p_paramsblock(p):
    '''paramsblock : params paramsblock
                   | COMMA params paramsblock
                   | empty'''

def p_params(p):
    '''params : type ID
              | empty'''
    global tempType
    if len(p) > 2 : 
        tempParams['params'][p[2]] = {'type' : tempType}


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
               | while
               | return'''

def p_cond(p):
    'cond : IF LPAREN express RPAREN LCURLY gotoif block RCURLY else'
    

def p_gotoif(p):
    'gotoif : empty'
    if ptypes[-1] == 'bool' : 
        x = pconsts.pop()
        xtyp = ptypes.pop()
        newQuad('GOTOF', x, '', '')

def p_else(p):
    '''else : ELSE LCURLY  gotoelse block RCURLY else
            | SEMICOLON'''

def p_gotoelse(p):
    'gotoelse : empty'
    newQuad('GOTO', '', '', '')

def p_assign(p):
    '''assign :  ID assign1 ASSIGN express SEMICOLON'''
    global tempVars
    x = p[1]
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if x in tempVars['varsTable'].keys() : 
        idtyp = tempVars['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            newQuad('=', '', rop, x)
    else: 
        sys.exit(p[1], ": variable not declared or not of a supported type.")


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
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    newQuad('fwd', lop, rop, '')

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

def p_while(p):
    '''while : WHILE LPAREN express RPAREN while1 LCURLY block RCURLY SEMICOLON'''
    global iQuads 
    jump = pjumps.pop()
    newQuad('GOTO', '', '', jump)
    quads[jump][3] = str(iQuads)

def p_while1(p):
    '''while1 : empty'''
    
    global iQuads 
    xtype = ptypes.pop()
    if xtype == 'bool' : 
        pjumps.append(iQuads-1)
        x = pconsts.pop()
        newQuad('GOTOF', x, '', '')

def p_return(p):
    'return : RETURN LPAREN express RPAREN SEMICOLON'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    newQuad('ret', rop , '', '')

def p_type(p):
    '''type : INT
            | FLOAT
            | BOOL
            | CHAR'''
    global tempType
    tempType = p[1]
        
def p_constant(p):
    '''constant : ID
                | CTE_INT
                | CTE_FLOAT
                | CTE_CHAR
                | CTE_BOOL'''
    global scope
    global pconsts
    global ptypes
    pconsts.append(p[1])
    print("appending", p[1])
    if p[1] in tempVars['varsTable'].keys() :
        ptypes.append(tempVars['varsTable'][p[1]]['type'])
    elif p[1] == 'true' or p[1] == 'false' :
        ptypes.append('bool')
    elif p[1].isalpha() and len(p[1] == 1): 
        ptypes.append('char')
    elif p[1].isnumeric() :
        ptypes.append('int')
    elif isfloat(p[1]) :
        ptypes.append('float')
    else : 
        sys.exit(p[1], ": variable not declared or of not supported type.")


def p_express(p):
    'express : express1 relational express2'
    
def p_express1(p):
    '''express1 : NOTEQUALS
                | empty'''

def p_express2(p):
    '''express2 : andor express
                | empty'''

def p_andor(p):
    '''andor : AND
             | OR'''
    if len(popers) > 0 :
        x = popers[-1]
    else :
        x = 'NULL'
    
    if x == 'AND' or x == 'OR' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newTemp(restyp)
            newQuad(ope, lop, rop, temp)

def p_relational(p):
    '''relational : exp relational1
                  | NOT'''

def p_relational1(p):
    '''relational1 : compare exp
                   | empty'''
    if len(popers) > 0 :
        x = popers[-1]
    else :
        x = 'NULL'

    if x == '>' or x == '<' or x == '==' or x == '!=' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newTemp(restyp)
            newQuad(ope, lop, rop, temp)

def p_compare(p):
    '''compare  : LESSTHAN
                | GREATERTHAN
                | EQUALS
                | NOTEQUALS'''
    popers.append(p[1])

def p_exp(p):
    'exp : term exp1'
    if len(popers) > 0 :
        x = popers[-1]
    else :
        x = 'NULL'

    if x == '+' or x == '-' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newTemp(restyp)
            newQuad(ope, lop, rop, temp)

def p_exp1(p):
    '''exp1 : plusminus exp 
            | empty'''

def p_plusminus(p):
    '''plusminus : PLUS 
                 | MINUS'''
    popers.append(p[1])

def p_term(p):
    'term :  factor term1'
    if len(popers) > 0 :
        x = popers[-1]
    else :
        x = 'NULL'
    
    if x == '*' or x == '/' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newTemp(restyp)
            newQuad(ope, lop, rop, temp)

def p_term1(p):
    '''term1 : multidivi term 
             | empty'''

def p_multidivi(p):
    '''multidivi : MULTI 
                 | DIVI'''
    popers.append(p[1])

def p_factor(p): 
    '''factor : LPAREN express RPAREN
              | constant'''

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print ("Illegal token", p)
    global compileFlag 
    compileFlag = False

parser = yacc.yacc()

filename = "docs/tests/prueba1.txt"
fp = codecs.open(filename,"r")
nextline = fp.read()
fp.close()
lexer.input(nextline)
parser.parse(nextline)

if compileFlag == True:
    print("Compiled succesfull")
    for x in funcTable.items():
        print(x)
    i = 0
    for x in quads :
        print (i, x)
        i = i + 1
    print("opers", popers)
    print("consts", pconsts)
    print("tips", ptypes)
    print("jumps", pjumps)
else:
    print("ERROR: Could not compile")


    