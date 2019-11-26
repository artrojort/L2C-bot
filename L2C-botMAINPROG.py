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
import ast

#from VMfunctions import forward, backward, turnLeft, turnRight, delay, servo, lights, display


#operadores
popers = []
#variables
pconsts = []
#tipo
ptypes = []
#saltos para GOTOS
pjumps = []
#pila para recursion de funciones
pfuncs = ['main']
pparams = []
preturns = []
#cuadruplos
quads = []
parrays = []

#temporales
tempindex = 0
tempQuad = []
tempID = ''



compileFlag = True
iQuads = 0
iParams = 0
iCalledParams = 0
scope = 'global'
funcTable = {'global' : {'type' : 'void', 'era' : {'int': 0, 'float': 0, 'bool': 0, 'char': 0}, 'tempera' : {'int': 0, 'float': 0, 'bool': 0, 'char': 0}, 'params' : '', 'paramsTable' : {}, 'varsTable' : {}, 'start' : ''}}
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
    
tempera =  {'int' : 0,
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

overflows = {'global' : {'int'   : 12001,
                    'float' : 13001,
                    'bool'  : 14001,
                    'char'  : 15001
                    },
        'local'  : {'int'   : 22001,
                    'float' : 23001,
                    'bool'  : 24001,
                    'char'  : 25001
                    },
        'temp'   : {'int'   : 32001,
                    'float' : 33001,
                    'bool'  : 34001,
                    'char'  : 35001
                    },
        'const'  : {'int'   : 42001,
                    'float' : 43001,
                    'bool'  : 44001,
                    'char'  : 45001
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
    global tempera
    address = dirMem['temp'][varType]
    dirMem['temp'][varType] = address + 1
    checkOverflow('temp', varType)
    virMem['temp'][varType].append(address)
    pconsts.append(address)
    ptypes.append(varType)
    tempera[varType] = tempera[varType] + 1
    return address

def memClear():
    global dirMem
    global iParams
    global era
    global tempera
    global funcTable

    iParams = 0

    if scope != 'global':
        funcTable[scope]['varsTable'] = {}

    era =  {'int' : 0,
        'float' : 0,
        'bool' : 0,
        'char' : 0}

    tempera =  {'int' : 0,
        'float' : 0,
        'bool' : 0,
        'char' : 0}

    dirMem['local']['int'] = 21001
    dirMem['local']['float'] = 22001
    dirMem['local']['bool'] = 23001
    dirMem['local']['char'] = 24001
    dirMem['temp']['int'] = 31001
    dirMem['temp']['float'] = 32001
    dirMem['temp']['bool'] = 33001
    dirMem['temp']['char'] = 34001

    virMem['local']['int'] = []
    virMem['local']['float'] = []
    virMem['local']['bool'] = []
    virMem['local']['char'] = []
    virMem['temp']['int'] = []
    virMem['temp']['float'] = []
    virMem['temp']['bool'] = []
    virMem['temp']['char'] = []

def checkOverflow(scope, typ):
    if dirMem[scope][typ] >= overflows[scope][typ]:
        msg = ">> ERROR: Memory overflow. Too many " + typ + "s declared in " + scope  + " memory."
        sys.exit(msg)
    
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
        errormsg = "ERROR: " + a + ope +  b + " is not a valid operation."
        print(errormsg)
        sys.exit(errormsg)
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

tokens = ['ASSIGN', 'PLUS', 'MINUS', 'MULTI', 'DIVI', 'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET', 'LCURLY', 'RCURLY', 'EQUALS', 'LESSTHAN', 'GREATERTHAN', 'NOTEQUALS', 'SEMICOLON', 'COMMA', 'AND', 'OR', 'NOT', 'CTE_BOOL', 'CTE_INT', 'CTE_FLOAT', 'CTE_CHAR', 'ID']  + list(reserved.values())
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
    memClear()

def p_gotomain(p):
    'gotomain : empty'
    newQuad('GOTO', '', '', '')

def p_main(p):
    'main : MAIN setmain LPAREN RPAREN LCURLY varsblock main1 block RCURLY'
    funcTable['main']['varsTable'] = {}

def p_main1(p):
    'main1 : empty'
    funcTable[scope]['era'] = era
    funcTable[scope]['tempera'] = tempera
    funcTable[scope]['params'] = iParams
    

def p_setmain(p):
    'setmain : empty'
    global scope 
    scope = 'main'
    funcTable[scope] = {'type' : 'void', 'era' : '', 'tempera' : '', 'params' : iParams, 'varsTable' : {}, 'start' : iQuads}
    quads[0][3] = iQuads

def p_funcsblock(p):
    '''funcsblock : funcs funcsblock 
                  | empty'''

def p_funcs(p):
    'funcs : FUNCDEF type setscope LPAREN paramsblock RPAREN LCURLY varsblock funcs1 block RCURLY'''
    
    newQuad('ENDPROC', '', '', '')
    memClear()    

def p_funcs1(p):
    '''funcs1 : empty'''
    funcTable[scope]['era'] = era
    funcTable[scope]['tempera'] = tempera
    funcTable[scope]['params'] = iParams
    
def p_setscope(p):
    '''setscope : ID'''
    global scope
    memClear()
    scope = p[1]
    funcTable[scope] = {'type' : tempType, 'era' : '', 'tempera' : '', 'params' : iParams, 'paramsTable' : {}, 'varsTable' : {}, 'start' : iQuads}
    if tempType != 'void':
        address = dirMem['global'][tempType]    
        dirMem['global'][tempType] = address + 1
        checkOverflow('global', tempType)
        virMem['global'][tempType].append(p[1])
        funcTable['global']['varsTable'][p[1]] = {'type' : tempType, 'address' : address, 'dim' : 1}

def p_varsblock(p):
    '''varsblock : vars varsblock 
                 | empty'''
    
def p_vars(p):
    'vars : VARDEF type ID dimvar SEMICOLON'
    global dirMem
    global virMem
    global tempVars
    global era
    x = p[3]
    print(p[4])
    erasize = 1
    if scope == 'global' :
        address = dirMem['global'][tempType]
        dirMem['global'][tempType] = address + 1
        checkOverflow('global', tempType)
        virMem['global'][tempType].append(x)
    else : 
        address = dirMem['local'][tempType]
        dirMem['local'][tempType] = address + 1
        checkOverflow('local', tempType)
        virMem['local'][tempType].append(x)

    if x not in funcTable[scope]['varsTable'].keys() and x not in funcTable['global']['varsTable'].keys() : 
        funcTable[scope]['varsTable'][x] = {'type' : tempType, 'address' : address, 'dim' : 1}
        if p[4] == '[' :
            dimsize = int(virMem['const']['int'][int(str(pconsts[-1])[2] + str(pconsts[-1])[3] + str(pconsts[-1])[4])-1])
            print(dimsize)
            if ptypes[-1] == 'int' and dimsize > 1:
                pconsts.pop() 
                ptypes.pop()
                funcTable[scope]['varsTable'][p[3]]['dim'] == dimsize
                erasize = dimsize
                print("HERE", funcTable[scope]['varsTable'])
            else:
                msg = ">> ERROR: dimension of array must be of type INT and > 1"
                sys.exit(msg)
        era[tempType] = era[tempType] + erasize
    else : 
        errorMsg = "ERROR: ID '" + x + "' already asigned to a parameter or variable"
        sys.exit(errorMsg)
    
def p_dimvar(p):
    '''dimvar : LBRACKET express RBRACKET 
              | empty'''
    p[0] = p[1]

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
    checkOverflow('local', tempType)
    if len(p) > 2 : 
        if x not in funcTable[scope]['varsTable'].keys() and x not in funcTable['global']['varsTable'].keys() : 
            funcTable[scope]['varsTable'][x] = {'type' : tempType, 'address' : address}
            funcTable[scope]['paramsTable'][iParams+1] = {'ID' : x, 'type' : tempType, 'address' : address}
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
    '''assign :  ID array ASSIGN express'''
    x = p[1]
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if x in funcTable[scope]['varsTable'].keys() : 
        idtyp = funcTable[scope]['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            if len(parrays) != 0 :
                dimpos = parrays.pop()
                newQuad('=', rop, '', funcTable[scope]['varsTable'][x]['address']+dimpos-1)
            else:
                newQuad('=', rop, '', funcTable[scope]['varsTable'][x]['address'])
            
    elif x in funcTable['global']['varsTable'].keys() : 
        idtyp = funcTable['global']['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            newQuad('=', rop, '', funcTable['global']['varsTable'][x]['address'])
    else: 
        errorMsg = str(p[1]) +  " : variable not declared or of not supported type."
        sys.exit(errorMsg)

def p_call(p):
    'call : era LPAREN insertfloor paramcall RPAREN endfloor'
    global paramCall
    global calledFunc
    jump = funcTable[calledFunc]['start']
    if len(paramCall.keys()) != funcTable[calledFunc]['params'] : 
        errorMsg = ("ERROR: Number of parameters don't match.")
        sys.exit(errorMsg)
    i = 1
    while i <= funcTable[calledFunc]['params'] :
        if paramCall[i]['type'] == funcTable[calledFunc]['paramsTable'][i]['type'] : 
            newQuad('PARAM', paramCall[i]['val'], '', funcTable[calledFunc]['paramsTable'][i]['address'])
        else:
            errorMsg = ("ERROR: Types of parameters don't match.")
            sys.exit(errorMsg)
        i = i + 1
    newQuad('GOSUB', calledFunc, '', jump)
    if funcTable[calledFunc]['type']  != 'void':
        temp = newAdd(funcTable[calledFunc]['type'])
        globaddress = funcTable['global']['varsTable'][calledFunc]['address']
        newQuad('=', globaddress, '', temp)
    
    paramCall = {}

def p_era(p):
    '''era : ID'''
    global iCalledParams
    global calledFunc
    calledFunc = p[1]
    iCalledParams = funcTable[calledFunc]['params']
    tera = []
    tera.append(funcTable[calledFunc]['era']['int'])
    tera.append(funcTable[calledFunc]['era']['float'])
    tera.append(funcTable[calledFunc]['era']['bool'])
    tera.append(funcTable[calledFunc]['era']['char'])
    if calledFunc in funcTable.keys() : 
        newQuad('ERA', calledFunc, '', tera)
    else : 
        errorMsg = "ERROR: function " + calledFunc + " hasn't been declared"
        sys.exit(errorMsg)

def p_paramcall(p):
    '''paramcall : express paramcall1
                 | empty'''
    
    global iCalledParams
    global paramCall
    global iParams
    if len(p) > 2 :
        param = pconsts.pop()
        paramtype = ptypes.pop()
        paramCall[iCalledParams] = {'type' : paramtype, 'val' : param}
        iCalledParams = iCalledParams - 1
    

def p_call1(p):
    '''paramcall1 : COMMA paramcall 
                  | empty'''

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
    'forward : FORWARD LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if ltyp == 'int':
        newQuad('FORWARD', lop, '', '')
    else : 
        errorMsg = "ERROR: expected int value, got " + ltyp + " instead."
        sys.exit(errorMsg)
    
def p_backward(p):
    'backward : BACKWARD LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if  ltyp == 'int':
        newQuad('BACKWARD', lop, '', '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + ltyp + " instead."
        sys.exit(errorMsg)

def p_turnleft(p):
    'turnleft : TURNLEFT LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if ltyp == 'int':
        newQuad('TURNLEFT', lop, '', '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + ltyp + " instead."
        sys.exit(errorMsg)

def p_turnright(p):
    'turnright : TURNRIGHT LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if ltyp == 'int':
        newQuad('TURNRIGHT', lop, '', '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + ltyp + " instead."
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


def p_distance(p):
    'distance : DISTANCE LPAREN RPAREN'
    newQuad('DISTANCE', '', '', '')

def p_stop(p):
    'stop : STOP LPAREN RPAREN'
    newQuad('STOP', '', '', '')

def p_while(p):
    '''while : WHILE LPAREN express RPAREN while1 LCURLY block RCURLY'''
    global iQuads 
    jump = pjumps.pop()
    newQuad('GOTO', '', '', jump)
    quads[jump+1][3] = iQuads

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
    retType = funcTable['global']['varsTable'][scope]['type']
    if rtyp == retType :
        address = funcTable['global']['varsTable'][scope]['address']
        newQuad('RETURN', rop, '', address)
        newQuad('ENDPROC', '', '', '')
        

def p_type(p):
    '''type : INT
            | FLOAT
            | BOOL
            | CHAR
            | VOID'''
    global tempType
    tempType = p[1]
        
def p_constant(p):
    '''constant : ID array
                | CTE_INT
                | CTE_FLOAT
                | CTE_CHAR
                | CTE_BOOL'''
    global pconsts
    global ptypes
    global dirMem
    global tempID
    if p[1] in funcTable[scope]['varsTable'].keys() :
        tempID = p[1]
        if len(parrays) != 0 :
            dimpos = parrays.pop()
            pconsts.append(funcTable[scope]['varsTable'][p[1]]['address']+dimpos-1)
        else :
            pconsts.append(funcTable[scope]['varsTable'][p[1]]['address'])
        ptypes.append(funcTable[scope]['varsTable'][p[1]]['type'])
    elif p[1] in funcTable['global']['varsTable'].keys() :
        tempID = p[1]
        ptypes.append(funcTable['global']['varsTable'][p[1]]['type'])
        pconsts.append(funcTable['global']['varsTable'][p[1]]['address'])
    elif p[1] == 'true' or p[1] == 'false' :
        address = dirMem['const']['bool']
        dirMem['const']['bool'] = address + 1
        checkOverflow('const', 'bool')
        virMem['const']['bool'].append(p[1])
        pconsts.append(address)
        ptypes.append('bool')
    elif p[1].isalpha() and len(p[1]) == 1: 
        if p[1] not in virMem['const']['char'] :
            address = dirMem['const']['char']
            dirMem['const']['char'] = address + 1
            checkOverflow('const', 'char')
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
            checkOverflow('const', 'int')
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
            checkOverflow('const', 'float')
            virMem['const']['float'].append(p[1])
        else : 
            pos = virMem['const']['float'].index(p[1])
            address = 42001 + pos
        pconsts.append(address)
        ptypes.append('float')
    else : 
        errorMsg = str(p[1]) +  " : variable not declared or of not supported type."
        sys.exit(errorMsg)

def p_array(p):
    '''array : LBRACKET insertfloor express RBRACKET endfloor
             | empty'''
    if p[1] == '[' :
        dimsize = int(virMem['const']['int'][int(str(pconsts[-1])[2] + str(pconsts[-1])[3] + str(pconsts[-1])[4])-1])
        print(dimsize)
        if ptypes[-1] == 'int':
            pconsts.pop() 
            ptypes.pop()
            parrays.append(dimsize)
        else:
            msg = ">> ERROR: position call of array must be of type INT"
            sys.exit(msg)
    

def p_express(p):
    'express : relational express1'

def p_express1(p):
    '''express1 : andor express
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
        print(pconsts)
        print(popers)
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newAdd(restyp)
            print(newQuad(ope, lop, rop, temp))

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
    '''factor : LPAREN insertfloor express RPAREN endfloor
              | constant
              | call'''
    
def p_insertfloor(p):
    '''insertfloor : empty'''
    popers.append('(')

def p_endfloor(p):
    '''endfloor : empty'''
    if popers[-1] == '(' :
        popers.pop()
    
def p_empty(p):
    'empty :'
    pass

def p_error(p):
    print ("Illegal token", p)
    global compileFlag 
    compileFlag = False

def memRead(dir):
    scopeFloor = 0
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
        if scope == 'local' : 
            scopeFloor = era[typ]
        val = virMem[scope][typ][pos + scopeFloor] 
        return int(val)
    elif dir[1] == '2':
        typ = 'float'
        if scope == 'local' : 
            scopeFloor = era[typ]
        val = virMem[scope][typ][pos + scopeFloor]
        return float(val)
    elif dir[1] == '3':
        typ = 'bool'
        if scope == 'local' : 
            scopeFloor = era[typ]
        val = virMem[scope][typ][pos + scopeFloor]
        return bool(val)
    elif dir[1] == '4':
        typ = 'char'
        if scope == 'local' : 
            scopeFloor = era[typ]
        val = virMem[scope][typ][pos + scopeFloor]
        return str(val)
    else :
        sys.exit("ERROR: Tried to access unassigned memory space")

def memWrite(val, dir):
    scopeFloor = 0
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
        if scope == 'local' : 
            scopeFloor = era[typ]
        virMem[scope][typ][pos + scopeFloor] = int(val)
    elif dir[1] == '2':
        typ = 'float'
        if scope == 'local' : 
            scopeFloor = era[typ]
        virMem[scope][typ][pos + scopeFloor] = float(val)
    elif dir[1] == '3':
        typ = 'bool'
        if scope == 'local' : 
            scopeFloor = era[typ]
        virMem[scope][typ][pos + scopeFloor] = bool(val)
    elif dir[1] == '4':
        typ = 'char'
        if scope == 'local' : 
            scopeFloor = era[typ]
        virMem[scope][typ][pos + scopeFloor] = str(val)
    
    return val

def newEra(esize):
    for _ in range(esize[0]):
        virMem['local']['int'].append('NON')
    
    for _ in range(esize[1]):
        virMem['local']['float'].append('NON')
    
    for _ in range(esize[2]):
        virMem['local']['bool'].append('NON')

    for _ in range(esize[3]):
        virMem['local']['char'].append('NON')

    for _ in range(10):
        virMem['temp']['int'].append('NON')
    
    for _ in range(10):
        virMem['temp']['float'].append('NON')
    
    for _ in range(10):
        virMem['temp']['bool'].append('NON')

    for _ in range(10):
        virMem['temp']['char'].append('NON')

def endEra(func):
    global era
    backToFunc = pfuncs[-1]
    eraFloor = funcTable[backToFunc]['era']
    era['int'] = era['int'] - eraFloor['int']
    era['float'] = era['float'] - eraFloor['float']
    era['bool'] = era['bool'] - eraFloor['bool']
    era['char'] = era['char'] - eraFloor['char']

    for _ in range(funcTable[func]['era']['int']):
        virMem['local']['int'].pop()
    
    for _ in range(funcTable[func]['era']['float']):
        virMem['local']['float'].pop()
    
    for _ in range(funcTable[func]['era']['bool']):
        virMem['local']['bool'].pop()

    for _ in range(funcTable[func]['era']['char']):
        virMem['local']['char'].pop()
    
    for _ in range(funcTable[func]['tempera']['int']):
        virMem['temp']['int'].append('NON')
    
    for _ in range(funcTable[func]['tempera']['float']):
        virMem['temp']['float'].append('NON')
    
    for _ in range(funcTable[func]['tempera']['bool']):
        virMem['temp']['bool'].append('NON')

    for _ in range(funcTable[func]['tempera']['char']):
        virMem['temp']['char'].append('NON')

def contextChange(func) :
    global era
    prevFunc = pfuncs[-2]
    eraFloor = funcTable[prevFunc]['era']
    era['int'] = era['int'] + eraFloor['int']
    era['float'] = era['float'] + eraFloor['float']
    era['bool'] = era['bool'] + eraFloor['bool']
    era['char'] = era['char'] + eraFloor['char']

    numParams = funcTable[func]['params']
    for _ in range(numParams) : 
        address = pparams.pop()
        val = pparams.pop()
        memWrite(val, address)

def virtualMachine() : 
    qPos = 0
    for _ in range(funcTable['main']['era']['int']):
        virMem['local']['int'].append('NON')
    
    for _ in range(funcTable['main']['era']['float']):
        virMem['local']['float'].append('NON')
    
    for _ in range(funcTable['main']['era']['bool']):
        virMem['local']['bool'].append('NON')

    for _ in range(funcTable['main']['era']['char']):
        virMem['local']['char'].append('NON')
    
    for _ in range(funcTable['main']['tempera']['int']):
        virMem['temp']['int'].append('NON')
    
    for _ in range(funcTable['main']['tempera']['float']):
        virMem['temp']['float'].append('NON')
    
    for _ in range(funcTable['main']['tempera']['bool']):
        virMem['temp']['bool'].append('NON')

    for _ in range(funcTable['main']['tempera']['char']):
        virMem['temp']['char'].append('NON')

    
    while qPos < len(quads) :
        print(quads[qPos])
        ope = quads[qPos][0]

        if ope == 'GOTO':
            qPos = quads[qPos][3]

        elif ope == 'GOTOF':
            lop = memRead(quads[qPos][1])
            if lop == False:
                qPos = quads[qPos][3]  
            else :
                qPos = qPos + 1
        
        elif ope == 'GOSUB':
            global tempQuad
            contextChange(quads[qPos][1])
            tempQuad.append(qPos + 1)
            qPos = quads[qPos][3]
        
        elif ope == 'PARAM':
            rop = memRead(quads[qPos][1])
            res = quads[qPos][3]
            pparams.append(rop)
            pparams.append(res)
            qPos = qPos + 1
            
        elif ope == 'ENDPROC':
            global era
            qPos = tempQuad.pop()
            backToFunc = pfuncs.pop()
            endEra(backToFunc)

        elif ope == 'ERA':
            erafunc = []
            erafunc = quads[qPos][3]
            newEra(erafunc)
            pfuncs.append(quads[qPos][1]) 
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
            msg = ">> " + str(rop)
            #display()
            print(msg)
            qPos = qPos + 1
        
        elif ope == 'LIGHTS' :
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            lights(lop, rop)
            qPos = qPos + 1
        
        elif ope == 'FORWARD' :
            lop = memRead(quads[qPos][1])
            forward(lop)
            qPos = qPos + 1
        
        elif ope == 'BACKWARD' :
            lop = memRead(quads[qPos][1])
            backward(lop)
            qPos = qPos + 1

        elif ope == 'TURNLEFT' :
            lop = memRead(quads[qPos][1])
            turnLeft(lop)
            qPos = qPos + 1
        
        elif ope == 'TURNRIGHT' :
            lop = memRead(quads[qPos][1])
            turnRight(lop)
            qPos = qPos + 1
        
        elif ope == 'SERVO' :
            lop = memRead(quads[qPos][1])
            servo(lop)
            qPos = qPos + 1
        
        elif ope == 'DELAY' :
            lop = memRead(quads[qPos][1])
            delay(lop)
            qPos = qPos + 1
        
        elif ope == 'RETURN' : 
            rop = memRead(quads[qPos][1])
            res = quads[qPos][3]
            memWrite(rop, res)
            qPos = qPos + 1

parser = yacc.yacc()
fileinput = input(">> Enter Filename: ")
filename = "docs/tests/" + fileinput  + ".txt"
fp = codecs.open(filename,"r")
nextline = fp.read()
fp.close()
lexer.input(nextline)
parser.parse(nextline)

midcode = open("midcode.l2c", "w")
if compileFlag == True:
    print(">> Compiled succesfull")
    for x in funcTable.items():
        print(x)
    for x in quads :
        midcode.write(str(x) + "\n")
        print(x)
    print("opers", popers)
    print("consts", pconsts)
    #print("tips", ptypes)
    #print("jumps", pjumps)
    #print (era)
    #print(virMem)
else:
    print(">> ERROR: Could not compile")
midcode.close()
print(">> Global memory:", virMem['global'])
print(">> Constant memory:", virMem['const'])
print(">> Program Start")
virtualMachine()
print(">> .")


    