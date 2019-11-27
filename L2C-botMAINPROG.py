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

#Import de funciones arduino. 
#Se requiere conectar el arduino para poder correr el compilador con el imports, o de otra forma se puede comentar la siguiente línea para ignorar el uso de arduino.
#from VMfunctions import forward, backward, turnLeft, turnRight, delay, servo, lights, display


#Pilas para manejo de prioridad
popers = [] #pila operadores
pconsts = [] #pila de variables
ptypes = [] #pila de tipos
pjumps = [] #pila de saltos
pfuncs = ['main'] #pila para cambios de contexto
pparams = [] #pila para lectura de parámetros
preturns = [] #pila para returns en cambios de contexto
parrays = [] #pila para dimensiones de arreglos
parraysid = [] #pila para ids de arreglos

#Arreglo de cuádrplos donde se almacena el código intermedio
quads = []

#Almacena el cúadruplo en el que se quedó la máquina virtual antes de un cambio de contexto 
tempQuad = []

#Bandera de compilación 
compileFlag = True

#Contadores
iQuads = 0 #Cuádruplo actual
iParams = 0 #Parámetro actual
iCalledParams = 0 #Número de parámetros llamados

#Referencias globales
scope = 'global' #Scope en el que se está trabajando durante compilación. Inicia como global.
thisID = '' #ID más reciente detectado
tempType = ''   #Tipo (int, bool, char) más reciente detectado
paramCall  = {} #Parámetros que se han llamado
calledFunc = '' #ID de función que se ha llamado

#Tabla de funciones
#Estructura de tabla funciones:
#ID función | tipo | era | era temporal | parámetros | tabla de variables | cuádruplo inicial  
#Estructura de tabla variables:
#ID | tipo | dirección | tamaño 
funcTable = {'global' : {'type' : 'void', 'era' : {'int': 0, 'float': 0, 'bool': 0, 'char': 0}, 'tempera' : {'int': 0, 'float': 0, 'bool': 0, 'char': 0}, 'params' : '', 'paramsTable' : {}, 'varsTable' : {}, 'start' : ''}}

#Diccionario para ERA de cada función
era =  {'int' : 0,
        'float' : 0,
        'char' : 0,
        'bool' : 0}

#Diccionario para ERA de temporales de cada función
tempera =  {'int' : 0,
        'float' : 0,
        'char' : 0,
        'bool' : 0}

#Indices iniciales de direcciones de memoria. Dividido en globales, locales, temporales y constantes. 
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

#Indices de cuando alguna memoria ya se encuentra fuera de rango, usado como referencia. 
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

#Memoria de la máquina virtual, en cada arreglo se van agregando las unidades necesarias para cada función. 
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

#Valida si un valor es flotante
def isfloat(value):
  try:
    float(value)
    return True
  except ValueError:
    return False

#Generación de un nuevo cúadruplo. Agrega el cuádruplo al stack e incrementa el contador.
def newQuad(ope, a, b, res):
    global iQuads
    quads.append([ope, a, b, res])
    iQuads = iQuads + 1

#Generación de nuevo temporal. Genera un temporal del tipo dado (varType) y lo agrega a la pila de operadores. 
def newAdd(varType):
    global dirMem
    global tempera
    address = dirMem['temp'][varType] #Revisa en cual indice de dirección vamos y se lo asigna al temporal generado 
    dirMem['temp'][varType] = address + 1 #Aumenta el índice de la dirección de temporales 
    checkOverflow('temp', varType) #Revisa que la dirección generada no sea un overflow
    pconsts.append(address)
    ptypes.append(varType)
    tempera[varType] = tempera[varType] + 1 #Aumenta el ERA de temporales de la función. 
    return address

#Función para limpiar todos los índices al empezar a leer una nueva función.
def memClear():
    global dirMem
    global iParams
    global era
    global tempera
    global funcTable

    iParams = 0

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

#Revisa si una dirección asignada se sale de su espacio. 
def checkOverflow(scope, typ):
    if dirMem[scope][typ] >= overflows[scope][typ]:
        msg = ">> ERROR: Memory overflow. Too many " + typ + "s declared in " + scope  + " memory."
        sys.exit(msg)
    
#Códigos de instrucciones
op = {
    'GOTO'      : 0,
    'GOTOF'     : 1,
    'GOSUB'     : 2,
    'PARAM'     : 3,
    'ENDPROC'   : 4,
    'VER'       : 5,
    'K'         : 6,
    'DIM'       : 7,
    'ERA'       : 8,
    '+'         : 9,
    '-'         : 10,
    '*'         : 11,
    '/'         : 12,
    '<'         : 13,
    '>'         : 14,
    '=='        : 15,
    'PRINT'     : 16,
    'LIGHTS'    : 17,
    'FORWARD'   : 18,
    'BACKWARD'  : 19,
    'TURNLEFT'  : 20,
    'TURNRIGHT' : 21,
    'SERVO'     : 22,
    'DELAY'     : 23,
    'RETURN'    : 24,
    'CIN'       : 25,
    '='         : 26,
    '!='        : 27
}

#Cubo Semántico
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

#Validación en cubo semántico.
#ope - Operación
#a - Operador izquierdo
#b - Operador derecho
def typeCheck(ope, a, b):
    try:
        x = semCube[a][b][ope]
        return x
    except KeyError:
        errormsg = ">> ERROR: " + a + ope +  b + " is not a valid operation."
        sys.exit(errormsg)
        return False

#Palabras reservadas
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
    'while'     : 'WHILE',
    'return'    : 'RETURN',
    'len'       : 'LEN',
    'fin'       : 'FIN',
    'int'       : 'INT',
    'float'     : 'FLOAT',
    'char'      : 'CHAR',
    'bool'      : 'BOOL',
    'void'      : 'VOID'
}

#Tokens
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
    print(">> ERROR: Caracter ilegal '%s'" % t.value[0])
    t.lexer.skip(1)

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    global thisID
    t.type = reserved.get(t.value,'ID')
    thisID = t.value
    return t

t_ignore = " \t"

def t_COMMENT(t):
    r'\#.*'
    pass

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")


lexer = lex.lex()

#GRAMÁTICA DE COMPILACIÓN
# PN = punto neurálgico

#Inicio de código
def p_program(p):
    'program : PROGRAM gotomain varsblock funcsblock main FIN SEMICOLON'
    memClear()

#PN: inserta cuádruplo vacío de GOTO para irse al main cuando empieze la ejecución
def p_gotomain(p):
    'gotomain : empty'
    newQuad(op['GOTO'], '', '', '')

#Estructura de main
def p_main(p):
    'main : MAIN setmain LPAREN RPAREN LCURLY varsblock main1 block RCURLY'

#PN: Terminado de leer las variables, se puede asignar los ERA y parametros del main a la tabla de funciones.
def p_main1(p):
    'main1 : empty'
    funcTable[scope]['era'] = era
    funcTable[scope]['tempera'] = tempera
    funcTable[scope]['params'] = iParams

#PN: Se cambia el scope a 'main' y se hace su tabla de funciones inicial.
def p_setmain(p):
    'setmain : empty'
    global scope 
    scope = 'main'
    funcTable[scope] = {'type' : 'void', 'era' : '', 'tempera' : '', 'params' : iParams, 'varsTable' : {}, 'start' : iQuads}
    quads[0][3] = iQuads

#Bloque de funciones donde se leen de 0 a muchas funciones
def p_funcsblock(p):
    '''funcsblock : funcs funcsblock 
                  | empty'''

#Estructura de función
def p_funcs(p):
    'funcs : FUNCDEF type setscope LPAREN paramsblock RPAREN LCURLY varsblock funcs1 block RCURLY'''
    newQuad(op['ENDPROC'], '', '', '')
    memClear()    

#PN: Terminado de leer las variables, se puede asignar los ERA y parametros del main a la tabla de funciones.
def p_funcs1(p):
    '''funcs1 : empty'''
    funcTable[scope]['era'] = era
    funcTable[scope]['tempera'] = tempera
    funcTable[scope]['params'] = iParams

#PN: Se cambia el scope al ID leído y se hace su tabla de funciones inicial. 
#    Si la función no es void, se genera una variable global con el ID del a función 
#    donde se almacenará el valor del return. 
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

#Bloque de variables donde se leen de 0 a muchas variables
def p_varsblock(p):
    '''varsblock : vars varsblock 
                 | empty'''

#Estructura de declaración de variable
def p_vars(p):
    'vars : VARDEF type ID dimvar SEMICOLON'
    global dirMem
    global virMem
    global era
    x = p[3]
    erasize = 1
    #Se empieza a clasificar la variable detectada. 
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
    #Si la variable no existe, se crea con la dirección calculada, de lo contrario se marca error. 
    if x not in funcTable[scope]['varsTable'].keys() and x not in funcTable['global']['varsTable'].keys() : 
        funcTable[scope]['varsTable'][x] = {'type' : tempType, 'address' : address, 'dim' : 1}
        if p[4] == '[' :
            dimsize = int(virMem['const']['int'][int(str(pconsts[-1])[2] + str(pconsts[-1])[3] + str(pconsts[-1])[4])-1])
            print(dimsize)
            if ptypes[-1] == 'int' and dimsize > 1:
                pconsts.pop() 
                ptypes.pop()
                funcTable[scope]['varsTable'][p[3]]['dim'] = dimsize
                erasize = dimsize
            else:
                msg = ">> ERROR: dimension of array must be of type INT and > 1"
                sys.exit(msg)
        era[tempType] = era[tempType] + erasize
    else : 
        errorMsg = ">> ERROR: ID '" + x + "' already assigned to a parameter or variable"
        sys.exit(errorMsg)

#PN: Si al declarar una variable se detecta un bracket, se avisa a la gramática de variables (p[0] = p[1]) para que le asigne también su tamaño   
def p_dimvar(p):
    '''dimvar : LBRACKET express RBRACKET 
              | empty'''
    p[0] = p[1]

#Bloque de parámetros donde van 0 o muchos parámetros. 
def p_paramsblock(p):
    '''paramsblock : params paramsblock
                   | COMMA params paramsblock
                   | empty'''

#Estructura de parámetros
def p_params(p):
    '''params : type ID
              | empty'''
    global tempType
    global iParams
    global dirMem
    global virMem
    global era
    x = p[2]

    address = dirMem['local'][tempType]
    dirMem['local'][tempType] = address + 1
    checkOverflow('local', tempType)
    if len(p) > 2 : 
        #Los parámetros se consideran una declaración de variables, entonces se anexan a la tabla de variables de la función. 
        #Tambien se anexan a la tabla de parámetros para poder identificarlos como tales. 
        if x not in funcTable[scope]['varsTable'].keys() and x not in funcTable['global']['varsTable'].keys() : 
            funcTable[scope]['varsTable'][x] = {'type' : tempType, 'address' : address}
            funcTable[scope]['paramsTable'][iParams+1] = {'ID' : x, 'type' : tempType, 'address' : address}
            era[tempType] = era[tempType] + 1
            virMem['local'][tempType].append(x)
            iParams = iParams + 1
        else : 
            errorMsg = ">> ERROR: ID '" + x + "' already asigned to a parameter or variable"
            sys.exit(errorMsg)

#Bloque de estatutos, itera sobre las posibles instrucciones de nuestro lenguaje. 
def p_block(p):
    '''block : statute SEMICOLON block
             | empty'''

#Estatutos de L2C-bot
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
               | while
               | return'''

#Estructura de estatuto condicional
def p_cond(p):
    'cond : IF LPAREN express RPAREN LCURLY gotoif block RCURLY else'
    global iQuads
    jump = pjumps.pop()
    quads[jump][3] = iQuads

#PN: Cuádruplo de GOTOIF para condicional
def p_gotoif(p):
    'gotoif : empty'
    global iQuads
    if ptypes[-1] == 'bool' : 
        x = pconsts.pop()
        ptypes.pop()
        pjumps.append(iQuads)
        newQuad(op['GOTOF'], x, '', '')

#Estructura de condicional else
def p_else(p):
    '''else : ELSE LCURLY gotoelse block RCURLY else
            | empty'''

#PN: Cuádruplo de GOTO para else
def p_gotoelse(p):
    'gotoelse : empty'
    global iQuads
    jump = pjumps.pop()
    pjumps.append(iQuads)
    newQuad(op['GOTO'], '', '', '')
    quads[jump][3] = iQuads

#Estatuto de asignación
def p_assign(p):
    '''assign : ID punto array ASSIGN express'''
    x = p[1]
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if x in funcTable[scope]['varsTable'].keys() : 
        idtyp = funcTable[scope]['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            if len(parrays) != 0 :
                dimsize = parrays.pop()
                idsize = funcTable[scope]['varsTable'][p[1]]['dim']
                basedir = funcTable[scope]['varsTable'][p[1]]['address']
                newQuad(op['VER'], dimsize, idsize, '')
                temp = newAdd('int')
                newQuad(op['K'], dimsize, -1, temp)
                dimension = pconsts.pop() 
                temp2 = newAdd('int')
                newQuad(op['DIM'], dimension, basedir, temp2)
                newQuad(op['='], rop, '', ('*'+str(temp2)))
            else:
                newQuad(op['='], rop, '', funcTable[scope]['varsTable'][x]['address'])
            
    elif x in funcTable['global']['varsTable'].keys() : 
        idtyp = funcTable['global']['varsTable'][x]['type']
        restyp = typeCheck('=', idtyp, rtyp)
        if restyp != False : 
            newQuad(op['='], rop, '', funcTable['global']['varsTable'][x]['address'])
    else: 
        errorMsg = str(p[1]) +  ">> ERROR : variable not declared or of not supported type."
        sys.exit(errorMsg)
    
#Estatuto de llamada a función
def p_call(p):
    'call : era LPAREN insertfloor paramcall RPAREN endfloor'
    global paramCall
    global calledFunc
    jump = funcTable[calledFunc]['start']
    if len(paramCall.keys()) != funcTable[calledFunc]['params'] : 
        errorMsg = (">> ERROR: Number of parameters don't match.")
        sys.exit(errorMsg)
    i = 1
    while i <= funcTable[calledFunc]['params'] :
        if paramCall[i]['type'] == funcTable[calledFunc]['paramsTable'][i]['type'] : 
            newQuad(op['PARAM'], paramCall[i]['val'], '', funcTable[calledFunc]['paramsTable'][i]['address'])
        else:
            errorMsg = (">> ERROR: Types of parameters don't match.")
            sys.exit(errorMsg)
        i = i + 1
    newQuad(op['GOSUB'], calledFunc, '', jump)
    if funcTable[calledFunc]['type']  != 'void':
        temp = newAdd(funcTable[calledFunc]['type'])
        globaddress = funcTable['global']['varsTable'][calledFunc]['address']
        newQuad(op['='], globaddress, '', temp)
    
    paramCall = {}

#PN: Se lee el ERA de la función llamada, para generar su cuádruplo de ERA
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
        newQuad(op['ERA'], calledFunc, '', tera)
    else : 
        errorMsg = ">> ERROR: function " + calledFunc + " hasn't been declared"
        sys.exit(errorMsg)

#Parámetros con los que se ha llamado la función. Se añaden a un diccionario temporal para después validar que concidan con los parámetros reales de la función. 
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
    
#Más parametros
def p_paramcall1(p):
    '''paramcall1 : COMMA paramcall 
                  | empty'''

#Estatuto de input
def p_cin(p):
    'cin : CIN LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    newQuad(op['CIN'], '', '', rop)

#Estatuto de output
def p_cout(p):
    'cout : COUT LPAREN express cout1 RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    newQuad(op['PRINT'], rop, '', '')

#Más expresiones de output
def p_cout1(p):
    '''cout1 : COMMA express cout1
             | empty'''
    if len(p) > 2:
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        newQuad(op['PRINT'], rop, '', '')

#Estatuto delay
#FUNCION ARDUINO
#Para generar un tiempo de espera entre instrucciones 
#delay(seconds);
def p_delay(p):
    'delay : DELAY LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if rtyp == 'int' :
        newQuad(op['DELAY'], rop, '', '')
    else : 
        errorMsg = ">> ERROR: delay() was called with a " + rtyp + " but only works with int values."
        sys.exit(errorMsg)

#Estatuto forward
#FUNCION ARDUINO
#Para mover al carro en dirección frontal
#forward(seconds);
def p_forward(p):
    'forward : FORWARD LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if ltyp == 'int':
        newQuad(op['FORWARD'], lop, '', '')
    else : 
        errorMsg = "ERROR: expected int value, got " + ltyp + " instead."
        sys.exit(errorMsg)

#Estatuto backward
#FUNCION ARDUINO
#Para mover al carro en dirección trasera
#backward(seconds);  
def p_backward(p):
    'backward : BACKWARD LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if  ltyp == 'int':
        newQuad(op['BACKWARD'], lop, '', '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + ltyp + " instead."
        sys.exit(errorMsg)

#Estatuto turnleft
#FUNCION ARDUINO
#Para dar vuelta a la izquierda
#turnleft(seconds);
def p_turnleft(p):
    'turnleft : TURNLEFT LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if ltyp == 'int':
        newQuad('TURNLEFT', lop, '', '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + ltyp + " instead."
        sys.exit(errorMsg)

#Estatuto turnright
#FUNCION ARDUINO
#Para dar vuelta a la derecha
#turnright(seconds);
def p_turnright(p):
    'turnright : TURNRIGHT LPAREN express RPAREN'
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if ltyp == 'int':
        newQuad(op['TURNRIGHT'], lop, '', '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + ltyp + " instead."
        sys.exit(errorMsg)

#Estatuto servo
#FUNCION ARDUINO
#Para posicionar al servomotor en el grado especificado
#servo(degrees);
def p_servo(p):
    'servo : SERVO LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    if rtyp == 'int':
        newQuad(op['SERVO'], rop, '', '')
    else : 
        errorMsg = "ERROR: expected int, got " + rtyp + " instead."
        sys.exit(errorMsg)

#Estatuto turnleft
#FUNCION ARDUINO
#Para seleccionar un led y aplicarle una acción (1: Prender, 2: apagar, 3: parpadear)
#lights(led, action);
def p_lights(p):
    'lights :  LIGHTS LPAREN express COMMA express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    lop = pconsts.pop()
    ltyp = ptypes.pop()
    if rtyp == 'int' and ltyp == 'int':
        newQuad(op['LIGHTS'], lop, rop, '')
    else : 
        errorMsg = "ERROR: expected int and int value, got " + rtyp + ", " + ltyp + " instead."
        sys.exit(errorMsg)

#Estatuto de ciclos
def p_while(p):
    '''while : WHILE LPAREN express RPAREN while1 LCURLY block RCURLY'''
    global iQuads 
    jump = pjumps.pop()
    newQuad(op['GOTO'], '', '', jump)
    quads[jump+1][3] = iQuads

#PN: Se agrega cuádruplo GOTOF con el cual se indica el fin del ciclo 
def p_while1(p):
    '''while1 : empty'''
    global iQuads 
    xtype = ptypes.pop()
    if xtype == 'bool' : 
        pjumps.append(iQuads-1)
        x = pconsts.pop()
        newQuad(op['GOTOF'], x, '', '')

#Estatuto return
#Asigna el valor de la expresión a la variable global con el nombre de la función. 
def p_return(p):
    'return : RETURN LPAREN express RPAREN'
    rop = pconsts.pop()
    rtyp = ptypes.pop()
    retType = funcTable['global']['varsTable'][scope]['type']
    if rtyp == retType :
        address = funcTable['global']['varsTable'][scope]['address']
        newQuad(op['RETURN'], rop, '', address)
        newQuad(op['ENDPROC'], '', '', '')
        
#Estatuto para recibir el tamaño de una variable dimensionada. 
def p_len(p): 
    '''len : LEN LPAREN ID RPAREN'''
    global dirMem
    global virMem
    x = p[3]
    if x in funcTable['global']['varsTable'].keys():
        length = funcTable['global']['varsTable'][x]['dim']
    elif x in funcTable[scope]['varsTable'].keys():
        length = funcTable[scope]['varsTable'][x]['dim']
    else: 
        msg = (">> ERROR: couldn't find called variable for len()")
        sys.exit(msg)

    length = str(length)
    if length not in virMem['const']['int'] :
        address = dirMem['const']['int']
        dirMem['const']['int'] = address + 1
        checkOverflow('const', 'int')
        virMem['const']['int'].append(length)
    else : 
        pos = virMem['const']['int'].index(length)
        address = 41001 + pos

    temp = newAdd('int')
    newQuad(op['='], address, '', temp)


#tempType almacena el tipo seleccionado para después usarse en las asignaciones
def p_type(p):
    '''type : INT
            | FLOAT
            | BOOL
            | CHAR
            | VOID'''
    global tempType
    tempType = p[1]
        
#Lectura de variables 
def p_constant(p):
    '''constant : ID punto array
                | CTE_INT
                | CTE_FLOAT
                | CTE_CHAR
                | CTE_BOOL'''
    global pconsts
    global ptypes
    global dirMem
    #Se tiene que ir clasificando la variable para averiguar su tipo
    #Primero se busca si ya está declarada como local
    if p[1] in funcTable[scope]['varsTable'].keys() :
        if len(parrays) != 0 :
            dimsize = parrays.pop()
            #idsize = parraysid.pop()
            idsize = funcTable[scope]['varsTable'][p[1]]['dim']
            basedir = funcTable[scope]['varsTable'][p[1]]['address']
            newQuad(op['VER'], dimsize, idsize, '')
            temp = newAdd('int')
            newQuad(op['K'], dimsize, -1, temp)
            dimension = pconsts.pop() 
            temp2 = newAdd('int')
            newQuad(op['DIM'], dimension, basedir, temp2)
            pconsts.append('*'+str(temp2))
        else :
            pconsts.append(funcTable[scope]['varsTable'][p[1]]['address'])
        ptypes.append(funcTable[scope]['varsTable'][p[1]]['type'])
    #Se busca si es declarada como global
    elif p[1] in funcTable['global']['varsTable'].keys() :
        ptypes.append(funcTable['global']['varsTable'][p[1]]['type'])
        pconsts.append(funcTable['global']['varsTable'][p[1]]['address'])
    #Se busca si contiene un booleano
    elif p[1] == 'true' or p[1] == 'false' :
        address = dirMem['const']['bool']
        dirMem['const']['bool'] = address + 1
        checkOverflow('const', 'bool')
        virMem['const']['bool'].append(p[1])
        pconsts.append(address)
        ptypes.append('bool')
    #Se busca si es un caracter
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
    #Si es numerico se considera como int
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
    #Si era un string con punto flotante, se valida si es un float
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
    #Si no se clasifico como ninguna, se declara error
    else : 
        errorMsg = str(p[1]) +  " : variable not declared or of not supported type."
        sys.exit(errorMsg)

#PN: al detectar un bracket se inserta un piso falso para evaluar la expresión 
def p_array(p):
    '''array : LBRACKET insertfloor express RBRACKET endfloor
             | empty'''
    if p[1] == '[' :
        if ptypes[-1] == 'int':
            dimsize = pconsts.pop() 
            ptypes.pop()
            parrays.append(dimsize)
            
        else:
            msg = ">> ERROR: position call of array must be of type INT"
            sys.exit(msg)

def p_punto(p):
    '''punto : empty'''
    #global thisID
    #dim = funcTable[scope]['varsTable'][thisID]['dim']
    #parraysid.append(dim)
    

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
            newQuad(op[ope], lop, rop, temp)

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
    #Si el tope de operandos es un relacional, se resuelve y se genera su cuádruplo
    if x == '>' or x == '<' or x == '==' or x == '!=' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newAdd(restyp)
            newQuad(op[ope], lop, rop, temp)

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
    #Si el tope de operandos es + o -, se resuelvle con su cuadruplo  
    if x == '+' or x == '-' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newAdd(restyp)
            newQuad(op[ope], lop, rop, temp)

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
    #Si el tope de operandos es * o /, se resuelvle con su cuadruplo  
    if x == '*' or x == '/' :
        rop = pconsts.pop()
        rtyp = ptypes.pop()
        lop = pconsts.pop()
        ltyp = ptypes.pop()
        ope = popers.pop()
        restyp = typeCheck(ope, ltyp, rtyp)
        if restyp != False :
            temp = newAdd(restyp)
            newQuad(op[ope], lop, rop, temp)

def p_term1(p):
    '''term1 : multidivi term 
             | empty'''

def p_multidivi(p):
    '''multidivi : MULTI 
                 | DIVI'''
    popers.append(p[1])

#Se termina de resolver la expresión con una nueva expresión, una constante, una llamada o la funcion especial len para obtener el tamaño de una variable
def p_factor(p): 
    '''factor : LPAREN insertfloor express RPAREN endfloor
              | constant
              | call 
              | len'''

#PN: Insert de piso falso en stack operaciones
def p_insertfloor(p):
    '''insertfloor : empty'''
    popers.append('(')

#PN: Pop de piso falso en stack de operaciones
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

#Lectura de memoria
#Recibe como parámetro una direccion y regresa el valor almacenado en la dirección. 
def memRead(dir):
    if type(dir) == str:
        value =int(dir.replace('*', ''))
        dir = memRead(value)

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

#Escritura de memoria
#Recibe como parámetro un valor y una dirección.
#Almacena en la dirección el valor recibido. 
def memWrite(val, dir):
    if type(dir) == str:
        value =int(dir.replace('*', ''))
        dir = memRead(value)
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

#Se realiza el ERA de la función en la que se estaba antes de un cambio de contexto. 
#De esta forma se respetan los espacios de memoria de la función anterior y la nueva función inicia con sus propios espacios. 
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

#Al regresarnos de un cambio de contexto, se libera la memoria utilizada por la función llamada. 
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

#Se crea el piso de cambio de contexto
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

#Función de máquina virtual 
def virtualMachine() : 
    #Iterador de cuádruplos
    qPos = 0

    #Al iniciar la máquina, se tiene que la memoria vacía entonces se agregan los espacios del ERA de la función main
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

    #SWITCH de ejecución de cuádruplos
    while qPos < len(quads) :
        #print("--", quads[qPos])
        #print(virMem)
        ope = quads[qPos][0]

        #GOTO
        if ope == 0:
            qPos = quads[qPos][3]

        #GOTOF
        elif ope == 1:
            lop = memRead(quads[qPos][1])
            if lop == False:
                qPos = quads[qPos][3]  
            else :
                qPos = qPos + 1
        
        #GOSUB
        elif ope == 2:
            global tempQuad
            contextChange(quads[qPos][1])
            tempQuad.append(qPos + 1)
            qPos = quads[qPos][3]
        
        #PARAM
        elif ope == 3:
            rop = memRead(quads[qPos][1])
            res = quads[qPos][3]
            pparams.append(rop)
            pparams.append(res)
            qPos = qPos + 1

        #ENDPROC    
        elif ope == 4:
            global era
            qPos = tempQuad.pop()
            backToFunc = pfuncs.pop()
            endEra(backToFunc)
        #VER
        elif ope == 5:
            lop = memRead(quads[qPos][1])
            rop = quads[qPos][2]
            if lop <= rop:
                qPos  = qPos + 1
            else:
                msg = ">> ERROR: index out of range"
                sys.exit(msg)
        #K
        elif ope == 6:
            lop = memRead(quads[qPos][1])
            rop = quads[qPos][2]
            res = lop - 1
            memWrite(res, quads[qPos][3])
            qPos = qPos + 1
        
        #DIM
        elif ope == 7:
            lop = memRead(quads[qPos][1])
            rop = quads[qPos][2]
            res = lop + rop
            memWrite(res, quads[qPos][3])
            qPos = qPos + 1

        #ERA
        elif ope == 8:
            erafunc = []
            erafunc = quads[qPos][3]
            newEra(erafunc)
            pfuncs.append(quads[qPos][1]) 
            qPos = qPos + 1

        #+         
        elif ope == 9:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop + rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1
        
        #-
        elif ope == 10:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop - rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1
        
        #*
        elif ope == 11:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop * rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        # /
        elif ope == 12:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop / rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        #=
        elif ope == 26:
            rop = memRead(quads[qPos][1])
            res = quads[qPos][3]
            memWrite(rop, res)
            qPos = qPos + 1

       #< 
        elif ope == 13:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop < rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        #>
        elif ope == 14:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop > rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1

        #==
        elif ope == 15:
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            val = lop == rop
            res = quads[qPos][3]
            memWrite(val, res)
            qPos = qPos + 1
        
        #PRINT
        elif ope == 16 :
            rop = memRead(quads[qPos][1])
            msg = ">> " + str(rop)
            #display()
            print(msg)
            qPos = qPos + 1

        #INPUT
        elif ope == 25 :
            res = quads[qPos][3]
            val = input("<< reading value: ")
            memWrite(val, res)
            qPos = qPos + 1     
        
        #LIGHTS
        elif ope == 17 :
            lop = memRead(quads[qPos][1])
            rop = memRead(quads[qPos][2])
            lights(lop, rop)
            qPos = qPos + 1
        
        #FORWARD
        elif ope == 18 :
            lop = memRead(quads[qPos][1])
            forward(lop)
            qPos = qPos + 1
        
        #BACKWARD
        elif ope == 19 :
            lop = memRead(quads[qPos][1])
            backward(lop)
            qPos = qPos + 1

        #TURNLEFT
        elif ope == 20 :
            lop = memRead(quads[qPos][1])
            turnLeft(lop)
            qPos = qPos + 1
        
        #TURNRIGHT
        elif ope == 21 :
            lop = memRead(quads[qPos][1])
            turnRight(lop)
            qPos = qPos + 1
        
        #SERVO
        elif ope == 22 :
            lop = memRead(quads[qPos][1])
            servo(lop)
            qPos = qPos + 1
        
        #DELAY
        elif ope == 23 :
            lop = memRead(quads[qPos][1])
            delay(lop)
            qPos = qPos + 1
        
        #RETURN
        elif ope == 24 : 
            rop = memRead(quads[qPos][1])
            res = quads[qPos][3]
            memWrite(rop, res)
            qPos = qPos + 1
        
        elif ope == 25 :
            print("hola")
#Activa prints de elementos de compilacion y máquina virtual durante ejecución para debugs. 
def debug():
    for x in funcTable.items():
        print(x)

    for x in quads :
        print(x)

    print(">> Operand Stack: ", popers)
    print(">> Constants Stack: ", pconsts)
    print(">> Types Stack: ", ptypes)
    print(">> Jumps Stack: ", pjumps)
    print(">> ERA: ", era)
    print(">> Global memory:", virMem['global'])
    print(">> Constant memory:", virMem['const'])

parser = yacc.yacc()
fileinput = input(">> Enter Filename: ")
filename = "docs/tests/" + fileinput  + ".txt"
fp = codecs.open(filename,"r")
nextline = fp.read()
fp.close()
lexer.input(nextline)
parser.parse(nextline)
midfile = fileinput + ".l2c"
#Se inicia escritura de código intermedio
midcode = open(midfile, "w")
if compileFlag == True:
    for x in quads :
        midcode.write(str(x) + "\n")
    midcode.write(str(virMem['global']) + "\n")
    midcode.write(str(virMem['const']) + "\n")
    print(">> Compiling succesfull")
else:
    msg = ">> ERROR: Could not compile file"
    sys.exit(msg)
midcode.close()
#Fin de código intermedio

#Comentar la siguiente línea para deshabilitar el modo debug
#debug()

print(">> Program Start")
#Corre máquina virtual
virtualMachine()
print(">> .")


    