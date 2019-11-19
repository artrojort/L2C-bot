#Maquina Virtual L2C-bot
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

f = open("midcode.l2c", "r")
virMem = []
quads = []
if f.mode == 'r':
    fl =f.readlines()
    for x in fl:
        quads.append(x.split())
    vir = list(fl)[-1].replace(" ", "").replace('[]', "['NON']").replace('][', '],[')
    gMem = ast.literal_eval("[" + vir + "]")
    virMem.append(gMem)
    vir = list(fl)[-3].replace(" ", "").replace('[]', "['NON']").replace('][', '],[')
    lMem = ast.literal_eval("[" + vir + "]")
    virMem.append(lMem)
    vir = list(fl)[-5].replace(" ", "").replace('[]', "['NON']").replace('][', '],[')
    cMem = ast.literal_eval("[" + vir + "]")
    virMem.append(cMem)
    print(virMem)
f.close()

def memRead(dir):
    dir = str(dir)
    scope = int(dir[0])-1
    typ = int(dir[1])-1
    pos = int(dir[2] + dir[3] + dir[4])-1
    val = virMem[scope][typ][pos]
    return val

print(memRead(21001))
    
    
qPos = 0
while qPos == len(quads) :
    ope = quads[qPos][0]
    if ope == 'GOTO ':
        qPos = quads[qPos][3]
    if ope == '+':
        rop = quads[qPos][1]
        lop = quads[qPos][2]
        res = quads[qPos][3]
