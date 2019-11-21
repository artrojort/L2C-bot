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


compileFlag = True
iQuads = 0
iParams = 0
iCalledParams = 0
scope = 'global'
funcTable = {'global' : {'type' : '', 'era' : [0,0,0,0], 'params' : '', 'paramsTable' : {}, 'varsTable' : {}, 'start' : ''}}
tempVars = {'varsTable' : {}} 
tempParams = {'paramsTable' : {}}
paramCall  = {}
tempType = ''
calledFunc = ''
cont = 0

era =  {'int' : 0,
        'float' : 0,
        'char' : 0,
        'bool' : 0}

dirMem = {'global' : {'int'   : 11001,
                    'float' : 12001,
                    'bool'  : 13001,
                    'char'  : 14001
                    },
        'local'  : {'int'   : 21001,
                    'float' : 22001,
                    'bool'  : 23001,
                    'char'  : 24001
                    },
        'temp'   : {'int'   : 31001,
                    'float' : 32001,
                    'bool'  : 33001,
                    'char'  : 34001
                    },
        'const'  : {'int'   : 41001,
                    'float' : 42001,
                    'bool'  : 43001,
                    'char'  : 44001
                    }
        }

virMem = {'global': {'int'  : [],
                    'float' : [],
                    'bool'  : [],
                    'char'  : []
                    },
        'local'  : {'int'   : [],
                    'float' : [],
                    'bool'  : [],
                    'char'  : []
                    },
        'temp'   : {'int'   : [],
                    'float' : [],
                    'bool'  : [],
                    'char'  : []
                    },
        'const'  : {'int'   : [],
                    'float' : [],
                    'bool'  : [],
                    'char'  : []
                    }
        }

def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False

def newQuad(ope, a, b, res):
    global iQuads
    quads.append([ope, a, b, res])
    iQuads = iQuads + 1

def newAdd(varType):
    global dirMem
    address = dirMem['temp'][varType]
    dirMem['temp'][varType] = address + 1
    virMem['temp'][varType].append(address)
    pconsts.append(address)
    ptypes.append(varType)
    return address

def tempDump():
    global dirMem
    global tempVars
    global tempParams
    global iParams
    global era
    
    iParams = 0

    tempVars = {'varsTable' : {}} 
    tempParams = {'paramsTable' : {}}

    era =  {'int' : 0,
        'float' : 0,
        'char' : 0,
        'bool' : 0}

    dirMem['local']['int'] = 21001
    dirMem['local']['float'] = 22001
    dirMem['local']['bool'] = 23001
    dirMem['local']['char'] = 24001
    dirMem['temp']['int'] = 31001
    dirMem['temp']['float'] = 32001
    dirMem['temp']['bool'] = 33001
    dirMem['temp']['char'] = 34001

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
                                 '!=' : 'bool',
                                 '='  : 'char'
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

reserved = {
    'program'   : 'PROGRAM',
    'main'      : 'MAIN',
    'vardef'    : 'VARDEF',
    'funcdef'   : 'FUNCDEF',
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
t_CTE_CHAR  = r'\'[a-zA-Z_]\''
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
    'program : PROGRAM gotomain varsblock funcsblock main FIN SEMICOLON'

def p_gotomain(p):
    'gotomain : empty'
    newQuad('GOTO', '', '', '')

def p_main(p):
    'main : MAIN setmain LPAREN RPAREN LCURLY varsblock block RCURLY'
    global scope
    global tempVars
    global iParams
    global dirMem

def p_setmain(p):
    'setmain : empty'
    global scope 
    scope = 'main'
    funcTable[scope] = {'type' : 'void', 'era' : era, 'params' : iParams, 'varsTable' : {}, 'start' : iQuads}
    quads[0][3] = iQuads


def p_funcsblock(p):
    '''funcsblock : funcs funcsblock 
                  | empty'''
    

def p_funcs(p):
    'funcs : FUNCDEF choosetype setscope LPAREN paramsblock RPAREN LCURLY varsblock block RCURLY'''
    funcTable[scope]['era'] = era
    funcTable[scope]['params'] = iParams
    tempDump()    
    

def p_setscope(p):
    '''setscope : ID'''
    global scope
    scope = p[1]
    funcTable[scope] = {'type' : tempType, 'era' : '', 'params' : iParams, 'paramsTable' : {}, 'varsTable' : {}, 'start' : iQuads}

def p_varsblock(p):
    '''varsblock : vars varsblock 
                 | empty'''
    
def p_vars(p):
    'vars : VARDEF type ID vars1 SEMICOLON'
    global dirMem
    global virMem
    global tempVars
    global era
    
    x = p[3]

    if scope == 'global' :
        address = dirMem['global'][tempType]
        dirMem['global'][tempType] = address + 1
        virMem['global'][tempType].append(x)
    else : 
        address = dirMem['local'][tempType]
        dirMem['local'][tempType] = address + 1
        virMem['local'][tempType].append(x)

    if x not in funcTable[scope]['varsTable'].keys() and x not in funcTable['global']['varsTable'].keys() : 
        funcTable[scope]['varsTable'][x] = {'type' : tempType, 'address' : address}
        era[tempType] = era[tempType] + 1
    else : 
        errorMsg = "ERROR: ID '" + x + "' already asigned to a parameter or variable"
        sys.exit(errorMsg)
    
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
    global tempParams
    global iParams
    global dirMem
    global virMem
    global era
    x = p[2]

    address = dirMem['local'][tempType]
    dirMem['local'][tempType] = address + 1
    if len(p) > 2 : 
        if x not in funcTable[scope]['varsTable'].keys() and x not in funcTable['global']['varsTable'].keys() : 
            funcTable[scope]['varsTable'][x] = {'type' : tempType, 'address' : address}
            funcTable[scope]['paramsTable'][iParams+1] = {'ID' : x, 'type' : tempType}
            era[tempType] = era[tempType] + 1
            virMem['local'][tempType].append(x)
            iParams = iParams + 1
        else : 
            errorMsg = "ERROR: ID '" + x + "' already asigned to a parameter or variable"
            sys.exit(errorMsg)

def p_block(p):
    '''block : statute SEMICOLON block
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
    global iQuads
    jump = pjumps.pop()
    quads[jump][3] = iQuads

def p_gotoif(p):
    'gotoif : empty'
    global iQuads
    if ptypes[-1] == 'bool' : 
        x = pconsts.pop()
        ptypes.pop()
        pjumps.append(iQuads)
        newQuad('GOTOF', x, '', '')

def p_else(p):
    '''else : ELSE LCURLY gotoelse block RCURLY else
            | empty'''

def p_gotoelse(p):
    'gotoelse : empty'
    global iQuads
    jump = pjumps.pop()
    pjumps.append(iQuads)
    newQuad('GOTO', '', '', '')
    quads[jump][3] = iQuads


def p_assign(p):
    '''assign :  ID assign1 ASSIGN express'''
    x = p[1]
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if x in funcTable[scope]['varsTable'].keys() : 
        idtyp = funcTable[scope]['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            newQuad('=', rop, '', funcTable[scope]['varsTable'][x]['address'])
    elif x in funcTable['global']['varsTable'].keys() : 
        idtyp = funcTable['global']['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            newQuad('=', rop, '', funcTable['global']['varsTable'][x]['address'])
    else: 
        errorMsg = str(p[1]) +  " : variable not declared or of not supported type."
        sys.exit(errorMsg)

def p_assign1(p):
    '''assign1 : LBRACKET express RBRACKET 
               | empty'''

def p_call(p):
    'call : era LPAREN paramcall RPAREN'
    
    global paramCall
    global calledFunc
    jump = funcTable[calledFunc]['start']
    if len(paramCall.keys()) != funcTable[calledFunc]['params'] : 
        errorMsg = ("ERROR: Number of parameters don't match.")
        sys.exit(errorMsg)
    i = 1
    while i <= funcTable[calledFunc]['params'] :
        if paramCall[i]['type'] == funcTable[calledFunc]['paramsTable'][i]['type'] : 
            newQuad('PARAM', paramCall[i]['val'], '', ('~param' + str(i)))
        else:
            errorMsg = ("ERROR: Types of parameters don't match.")
            sys.exit(errorMsg)
        i = i + 1
    newQuad('GOSUB', calledFunc, '', jump)
    if funcTable[calledFunc]['type']  != 'void':
        temp = newAdd(funcTable[calledFunc]['type'])
        newQuad('=', calledFunc, '', temp)
        pconsts.append(temp)
        ptypes.append(funcTable[calledFunc]['type'])
    
    paramCall = {}

def p_era(p):
    '''era : ID'''
    global iCalledParams
    global calledFunc
    calledFunc = p[1]
    iCalledParams = funcTable[calledFunc]['params']
    era = []
    era.append(funcTable[calledFunc]['era']['int'])
    era.append(funcTable[calledFunc]['era']['float'])
    era.append(funcTable[calledFunc]['era']['char'])
    era.append(funcTable[calledFunc]['era']['bool'])
    if calledFunc in funcTable.keys() : 
        newQuad('ERA', calledFunc, '', str(era))
    else : 
        errorMsg = "ERROR: function " + calledFunc + " hasn't been declared"
        sys.exit(errorMsg)

def p_paramcall(p):
    '''paramcall : express paramcall1'''
    
    global iCalledParams
    global paramCall
    global iParams
    param = pconsts.pop()
    paramtype = ptypes.pop()
    paramCall[iCalledParams] = {'type' : paramtype, 'val' : param}
    iCalledParams = iCalledParams - 1
    

def p_call1(p):
    '''paramcall1 : COMMA paramcall 
                  | empty'''
    global iCalledParams

def p_cin(p):
    'cin : CIN cin1 '

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
    'cout : COUT LPAREN express cout1 RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    newQuad('PRINT', rop, '', '')

def p_cout1(p):
    '''cout1 : COMMA express cout1
             | empty'''
    if len(p) > 2:
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        newQuad('PRINT', rop, '', '')

def p_delay(p):
    'delay : DELAY LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if rtyp == 'int' :
        newQuad('DELAY', rop, '', '')
    else : 
        errorMsg = "ERROR: delay() was called with a " + rtyp + " but only works with int values."
        sys.exit(errorMsg)
 

def p_forward(p):
    'forward : FORWARD LPAREN express COMMA express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if rtyp == 'int' and ltyp == 'int':
        newQuad('FORWD', lop, rop, '')
    else : 
        errorMsg = "ERROR: expected int value, got " + rtyp + " instead."
        sys.exit(errorMsg)
    

def p_backward(p):
    'backward : BACKWARD LPAREN express COMMA express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if rtyp == 'int' and ltyp == 'int':
        newQuad('BACKWD', lop, rop, '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + rtyp + ", " + ltyp + " instead."
        sys.exit(errorMsg)


def p_turnleft(p):
    'turnleft : TURNLEFT LPAREN express COMMA express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if rtyp == 'int' and ltyp == 'int':
        newQuad('', lop, rop, '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + rtyp + ", " + ltyp + " instead."
        sys.exit(errorMsg)

def p_turnright(p):
    'turnright : TURNRIGHT LPAREN express COMMA express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if rtyp == 'int' and ltyp == 'int':
        newQuad('TURNRIGHT', lop, rop, '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + rtyp + ", " + ltyp + " instead."
        sys.exit(errorMsg)

def p_servo(p):
    'servo : SERVO LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if rtyp == 'int':
        newQuad('SERVO', rop, '', '')
    else : 
        errorMsg = "ERROR: expected int, got " + rtyp + " instead."
        sys.exit(errorMsg)

def p_lights(p):
    'lights :  LIGHTS LPAREN express COMMA express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if rtyp == 'int' and ltyp == 'int':
        newQuad('LIGHTS', lop, rop, '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + rtyp + ", " + ltyp + " instead."
        sys.exit(errorMsg)

def p_display(p):
    'display : DISPLAY LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    newQuad('DISPLAY', rop, '', '')

def p_distance(p):
    'distance : DISTANCE LPAREN RPAREN'
    newQuad('DISPLAY', '', '', '')

def p_stop(p):
    'stop : STOP LPAREN RPAREN'
    newQuad('STOP', '', '', '')

def p_while(p):
    '''while : WHILE LPAREN express RPAREN while1 LCURLY block RCURLY'''
    global iQuads 
    jump = pjumps.pop()
    newQuad('GOTO', '', '', jump)
    quads[jump+1][3] = str(iQuads)

def p_while1(p):
    '''while1 : empty'''
    global iQuads 
    xtype = ptypes.pop()
    if xtype == 'bool' : 
        pjumps.append(iQuads-1)
        x = pconsts.pop()
        newQuad('GOTOF', x, '', '')

def p_return(p):
    'return : RETURN LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    newQuad('RETURN', '', '', rop)
    


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
    global pconsts
    global ptypes
    global dirMem

    if p[1] in funcTable[scope]['varsTable'].keys() :
        ptypes.append(funcTable[scope]['varsTable'][p[1]]['type'])
        pconsts.append(funcTable[scope]['varsTable'][p[1]]['address'])
    elif p[1] in funcTable['global']['varsTable'].keys() :
        ptypes.append(funcTable['global']['varsTable'][p[1]]['type'])
        pconsts.append(funcTable['global']['varsTable'][p[1]]['address'])
    elif p[1] == 'true' or p[1] == 'false' :
        address = dirMem['const']['bool']
        dirMem['const']['bool'] = address + 1
        virMem['const']['bool'].append(p[1])
        pconsts.append(address)
        ptypes.append('bool')
    elif p[1].isalpha() and len(p[1]) == 1: 
        if p[1] not in virMem['const']['char'] :
            address = dirMem['const']['char']
            dirMem['const']['char'] = address + 1
            virMem['const']['char'].append(p[1])
        else : 
            pos = virMem['const']['char'].index(p[1])
            address = 43001 + pos
        pconsts.append(address)
        ptypes.append('char')
    elif p[1].isnumeric() :
        if p[1] not in virMem['const']['int'] :
            address = dirMem['const']['int']
            dirMem['const']['int'] = address + 1
            virMem['const']['int'].append(p[1])
        else : 
            pos = virMem['const']['int'].index(p[1])
            address = 41001 + pos
        pconsts.append(address)
        ptypes.append('int')
    elif isfloat(p[1]) :
        if p[1] not in virMem['const']['float'] :
            address = dirMem['const']['float']
            dirMem['const']['float'] = address + 1
            virMem['const']['float'].append(p[1])
        else : 
            pos = virMem['const']['float'].index(p[1])
            address = 42001 + pos
        pconsts.append(address)
        ptypes.append('float')
    else : 
        errorMsg = str(p[1]) +  " : variable not declared or of not supported type."
        sys.exit(errorMsg)

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
            temp = newAdd(restyp)
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
            temp = newAdd(restyp)
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
            temp = newAdd(restyp)
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
            temp = newAdd(restyp)
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
              | constant
              | call'''

def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print ("Illegal token", p)
    global compileFlag 
    compileFlag = False

def memRead(dir):

    dir = str(dir)
    pos = int(dir[2] + dir[3] + dir[4])-1

    if dir[0] == '1':
        scope = 'global'
    elif dir[0] == '2':
        scope = 'local'
    elif dir[0] =='3':
        scope = 'temp'
    elif dir[0] == '4':
        scope = 'const'
    
    if dir[1] == '1':
        typ = 'int'
        val = virMem[scope][typ][pos]
        return int(val)
    elif dir[1] == '2':
        typ = 'float'
        val = virMem[scope][typ][pos]
        return float(val)
    elif dir[1] == '3':
        typ = 'bool'
        val = virMem[scope][typ][pos]
        return bool(val)
    elif dir[1] == '4':
        typ = 'char'
        val = virMem[scope][typ][pos]
        return str(val)
    else :
        sys.exit("ERROR: Tried to access unassigned memory space")

def memWrite(val, dir):
    dir = str(dir)
    pos = int(dir[2] + dir[3] + dir[4])-1
    if dir[0] == '1':
        scope = 'global'
    elif dir[0] == '2':
        scope = 'local'
    elif dir[0] =='3':
        scope = 'temp'
    elif dir[0] == '4':
        scope = 'const'
    
    if dir[1] == '1':
        typ = 'int'
        virMem[scope][typ][pos] = int(val)
    elif dir[1] == '2':
        typ = 'float'
        virMem[scope][typ][pos] = float(val)
    elif dir[1] == '3':
        typ = 'bool'
        virMem[scope][typ][pos] = bool(val)
    elif dir[1] == '4':
        typ = 'char'
        virMem[scope][typ][pos] = str(val)
    
    return val

def virtualMachine() : 
    qPos = 0
    while qPos < len(quads) :
        ope = quads[qPos][0]
        if ope == 'GOTO':
            qPos = quads[qPos][3]

        if ope == 'GOTOF':
            lop = memRead(quads[qPos][1])
            print(lop)
            if lop == False:
                qPos = quads[qPos][3]  
            else :
                qPos = qPos + 1

        elif ope == '+':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop + rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1
        
        elif ope == '-':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop - rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1
        
        elif ope == '*':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop * rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        elif ope == '/':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop / rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        elif ope == '=':
            rop = memRead(quads[qPos][1])
            res = quads[qPos][3]
            memWrite(rop, res)
            qPos = qPos + 1
        
        elif ope == '<':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop < rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        elif ope == '>':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop > rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        elif ope == '==':
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop == rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1
        
        elif ope == 'PRINT' :
            rop = memRead(quads[qPos][1])
            print("PRINT")
            print(rop)
            qPos = qPos + 1


parser = yacc.yacc()
fileinput = input("FILENAME: ")
filename = "docs/tests/" + fileinput  + ".txt"
fp = codecs.open(filename,"r")
nextline = fp.read()
fp.close()
lexer.input(nextline)
parser.parse(nextline)

midcode = open("midcode.l2c", "w")
if compileFlag == True:
    print("Compiled succesfull")
    for x in funcTable.items():
        print(x)
    for x in quads :
        midcode.write(str(x) + "\n")
        print(x)
    print("opers", popers)
    print("consts", pconsts)
    print("tips", ptypes)
    print("jumps", pjumps)
    print (era)
    print(virMem)
else:
    print("ERROR: Could not compile")
virtualMachine()
midcode.close()

print(virMem)


    