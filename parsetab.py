
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND ASSIGN BACKWARD BOOL CALL CHAR CIN COMMA COUT CTE_ARR CTE_BOOL CTE_CHAR CTE_FLOAT CTE_INT DELAY DISPLAY DISTANCE DIVI ELSE EQUALS FIN FLOAT FORWARD FUNCDEF GREATERTHAN ID IF INT LBRACKET LCURLY LESSTHAN LIGHTS LPAREN MAIN MINUS MULTI NOT NOTEQUALS OR PLUS PROGRAM RBRACKET RCURLY RETURN RPAREN SEMICOLON SERVO STOP TURNLEFT TURNRIGHT VARDEF VOID WHILEprogram : PROGRAM gotomain globalvarsblock globalstartingquad funcsblock main FIN SEMICOLONgotomain : emptyglobalstartingquad : emptymain : MAIN LPAREN RPAREN LCURLY main2main2 : varsblock startingquad block RCURLYfuncsblock : funcs funcsblock \n                  | emptyfuncs : FUNCDEF choosetype ID LPAREN paramsblock RPAREN LCURLY varsblock startingquad block RCURLYglobalvarsblock : vars varsblock \n                       | emptyvarsblock : vars varsblock \n                 | emptystartingquad : emptyvars : VARDEF type ID vars1 SEMICOLONvars1 : LBRACKET CTE_INT RBRACKET \n             | emptychoosetype : type\n                  | VOIDparamsblock : params paramsblock\n                   | COMMA params paramsblock\n                   | emptyparams : type ID\n              | emptyblock : statute block\n             | emptystatute : cond\n               | assign\n               | call \n               | cin\n               | cout\n               | delay\n               | forward\n               | backward\n               | turnleft\n               | turnright\n               | servo\n               | lights\n               | display\n               | distance\n               | stop\n               | while\n               | returncond : IF LPAREN express RPAREN LCURLY gotoif block RCURLY elsegotoif : emptyelse : ELSE LCURLY  gotoelse block RCURLY else\n            | SEMICOLONgotoelse : emptyassign :  ID assign1 ASSIGN express SEMICOLONassign1 : LBRACKET express RBRACKET \n               | emptycall : era LPAREN paramcall RPAREN SEMICOLONera : IDparamcall : express paramcall1paramcall1 : COMMA paramcall \n                  | emptycin : CIN cin1 SEMICOLONcin1 : cin2 \n            | cin3cin2 : LPAREN ID RPARENcin3 : LBRACKET CTE_INT RBRACKET LPAREN cin4 RPARENcin4 : COMMA ID cin4 \n           | emptycout : COUT LPAREN express RPAREN SEMICOLONdelay : DELAY LPAREN CTE_INT RPAREN SEMICOLONforward : FORWARD LPAREN express COMMA express RPAREN SEMICOLONbackward : BACKWARD LPAREN express COMMA express RPAREN SEMICOLONturnleft : TURNLEFT LPAREN express COMMA express RPAREN SEMICOLONturnright : TURNRIGHT LPAREN express COMMA express RPAREN SEMICOLONservo : SERVO LPAREN express RPAREN SEMICOLONlights :  LIGHTS LPAREN CTE_INT COMMA CTE_INT RPAREN SEMICOLONdisplay : DISPLAY LPAREN CTE_CHAR RPAREN SEMICOLONdistance : DISTANCE LPAREN RPAREN SEMICOLONstop : STOP LPAREN RPAREN SEMICOLONwhile : WHILE LPAREN express RPAREN while1 LCURLY block RCURLY SEMICOLONwhile1 : emptyreturn : RETURN LPAREN express RPAREN SEMICOLONtype : INT\n            | FLOAT\n            | BOOL\n            | CHARconstant : ID\n                | CTE_INT\n                | CTE_FLOAT\n                | CTE_CHAR\n                | CTE_BOOLexpress : express1 relational express2express1 : NOTEQUALS\n                | emptyexpress2 : andor express\n                | emptyandor : AND\n             | ORrelational : exp relational1\n                  | NOTrelational1 : compare exp\n                   | emptycompare  : LESSTHAN\n                | GREATERTHAN\n                | EQUALS\n                | NOTEQUALSexp : term exp1exp1 : plusminus exp \n            | emptyplusminus : PLUS \n                 | MINUSterm :  factor term1term1 : multidivi term \n             | emptymultidivi : MULTI \n                 | DIVIfactor : LPAREN express RPAREN\n              | constantempty :'
    
_lr_action_items = {'PROGRAM':([0,],[2,]),'$end':([1,39,],[0,-1,]),'VARDEF':([2,3,4,6,11,37,43,58,],[-113,8,-2,8,8,-14,8,8,]),'FUNCDEF':([2,3,4,5,6,7,9,10,11,12,13,20,23,37,183,],[-113,-113,-2,-113,-113,-10,22,-3,-113,-9,-12,22,-11,-14,-8,]),'MAIN':([2,3,4,5,6,7,9,10,11,12,13,19,20,21,23,27,37,183,],[-113,-113,-2,-113,-113,-10,-113,-3,-113,-9,-12,26,-113,-7,-11,-6,-14,-8,]),'INT':([8,22,41,45,46,47,53,54,55,],[15,15,15,15,15,-23,15,-23,-22,]),'FLOAT':([8,22,41,45,46,47,53,54,55,],[16,16,16,16,16,-23,16,-23,-22,]),'BOOL':([8,22,41,45,46,47,53,54,55,],[17,17,17,17,17,-23,17,-23,-22,]),'CHAR':([8,22,41,45,46,47,53,54,55,],[18,18,18,18,18,-23,18,-23,-22,]),'IF':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,80,-13,-113,80,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,80,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,80,-44,80,-65,-66,-67,-68,-70,-43,-46,-74,-113,80,-47,-45,]),'ID':([11,13,14,15,16,17,18,23,28,29,30,37,43,48,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,100,102,104,108,110,112,113,114,115,116,121,122,123,125,126,127,128,132,155,166,172,173,174,175,179,180,184,186,188,189,191,193,194,195,196,198,200,201,203,205,206,208,209,212,213,218,220,223,224,225,232,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,24,-77,-78,-79,-80,-11,36,-17,-18,-14,-113,55,-113,81,-13,-113,81,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,-113,-113,-113,133,-113,-113,-113,-113,-113,-113,-113,-113,81,157,-87,-88,-113,-56,-113,-113,-113,-113,-113,-113,-72,-73,-113,-113,-91,-92,157,-97,-98,-99,-100,157,-104,-105,157,-109,-110,-48,-51,-63,-64,-69,-71,-76,81,-44,242,81,-65,-66,-67,-68,-70,-43,-46,-74,-113,81,-47,-45,]),'CIN':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,83,-13,-113,83,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,83,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,83,-44,83,-65,-66,-67,-68,-70,-43,-46,-74,-113,83,-47,-45,]),'COUT':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,84,-13,-113,84,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,84,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,84,-44,84,-65,-66,-67,-68,-70,-43,-46,-74,-113,84,-47,-45,]),'DELAY':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,85,-13,-113,85,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,85,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,85,-44,85,-65,-66,-67,-68,-70,-43,-46,-74,-113,85,-47,-45,]),'FORWARD':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,86,-13,-113,86,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,86,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,86,-44,86,-65,-66,-67,-68,-70,-43,-46,-74,-113,86,-47,-45,]),'BACKWARD':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,87,-13,-113,87,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,87,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,87,-44,87,-65,-66,-67,-68,-70,-43,-46,-74,-113,87,-47,-45,]),'TURNLEFT':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,88,-13,-113,88,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,88,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,88,-44,88,-65,-66,-67,-68,-70,-43,-46,-74,-113,88,-47,-45,]),'TURNRIGHT':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,89,-13,-113,89,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,89,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,89,-44,89,-65,-66,-67,-68,-70,-43,-46,-74,-113,89,-47,-45,]),'SERVO':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,90,-13,-113,90,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,90,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,90,-44,90,-65,-66,-67,-68,-70,-43,-46,-74,-113,90,-47,-45,]),'LIGHTS':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,91,-13,-113,91,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,91,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,91,-44,91,-65,-66,-67,-68,-70,-43,-46,-74,-113,91,-47,-45,]),'DISPLAY':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,92,-13,-113,92,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,92,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,92,-44,92,-65,-66,-67,-68,-70,-43,-46,-74,-113,92,-47,-45,]),'DISTANCE':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,93,-13,-113,93,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,93,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,93,-44,93,-65,-66,-67,-68,-70,-43,-46,-74,-113,93,-47,-45,]),'STOP':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,94,-13,-113,94,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,94,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,94,-44,94,-65,-66,-67,-68,-70,-43,-46,-74,-113,94,-47,-45,]),'WHILE':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,95,-13,-113,95,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,95,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,95,-44,95,-65,-66,-67,-68,-70,-43,-46,-74,-113,95,-47,-45,]),'RETURN':([11,13,23,37,43,50,56,57,58,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,123,132,179,180,184,208,209,212,213,218,220,223,224,225,239,243,244,245,246,247,252,254,255,256,257,258,261,],[-113,-12,-11,-14,-113,-113,96,-13,-113,96,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,96,-56,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,96,-44,96,-65,-66,-67,-68,-70,-43,-46,-74,-113,96,-47,-45,]),'RCURLY':([11,13,23,37,43,50,56,57,58,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,97,99,123,132,148,179,180,184,208,209,212,213,218,220,223,224,225,239,240,243,244,245,246,247,248,252,254,255,256,257,258,259,261,],[-113,-12,-11,-14,-113,-113,-113,-13,-113,98,-113,-25,-26,-27,-28,-29,-30,-31,-32,-33,-34,-35,-36,-37,-38,-39,-40,-41,-42,-113,-24,-113,-56,183,-72,-73,-113,-48,-51,-63,-64,-69,-71,-76,-113,-44,-113,249,-65,-66,-67,-68,-70,251,-43,-46,-74,-113,-113,-47,260,-45,]),'VOID':([22,],[30,]),'LBRACKET':([24,81,83,],[32,102,109,]),'SEMICOLON':([24,31,33,34,42,105,106,107,144,145,150,151,152,153,154,156,157,158,159,160,161,162,164,168,170,171,176,178,182,185,187,190,192,197,199,202,204,226,227,228,229,230,234,235,236,237,238,241,249,251,260,],[-113,37,-16,39,-15,132,-57,-58,179,180,-113,-113,-94,-113,-113,-112,-81,-82,-83,-84,-85,208,209,-59,212,213,218,220,223,-86,-90,-93,-96,-101,-103,-106,-108,-89,-95,-102,-107,-111,243,244,245,246,247,-60,254,255,254,]),'FIN':([25,49,98,],[34,-4,-5,]),'LPAREN':([26,36,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,100,102,104,110,112,113,114,115,116,121,122,125,126,127,128,155,166,169,172,173,174,175,186,188,189,191,193,194,195,196,198,200,201,203,205,206,],[35,41,100,-52,104,108,110,111,112,113,114,115,116,117,118,119,120,121,122,-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,155,-87,-88,-113,-113,-113,211,-113,-113,-113,-113,-113,-91,-92,155,-97,-98,-99,-100,155,-104,-105,155,-109,-110,]),'CTE_INT':([32,100,102,104,109,110,111,112,113,114,115,116,117,121,122,125,126,127,128,155,166,172,173,174,175,177,186,188,189,191,193,194,195,196,198,200,201,203,205,206,],[38,-113,-113,-113,134,-113,136,-113,-113,-113,-113,-113,142,-113,-113,158,-87,-88,-113,-113,-113,-113,-113,-113,-113,219,-113,-91,-92,158,-97,-98,-99,-100,158,-104,-105,158,-109,-110,]),'RPAREN':([35,41,44,45,46,47,52,53,54,55,59,119,120,124,130,131,133,135,136,141,143,146,147,150,151,152,153,154,156,157,158,159,160,161,165,167,185,187,190,192,197,199,202,204,207,210,211,214,215,216,217,219,226,227,228,229,230,231,233,242,250,],[40,-113,51,-113,-113,-21,-19,-113,-23,-22,-20,144,145,149,164,-113,168,170,171,176,178,181,182,-113,-113,-94,-113,-113,-112,-81,-82,-83,-84,-85,-53,-55,-86,-90,-93,-96,-101,-103,-106,-108,230,-54,-113,234,235,236,237,238,-89,-95,-102,-107,-111,241,-62,-113,-61,]),'RBRACKET':([38,129,134,150,151,152,153,154,156,157,158,159,160,161,185,187,190,192,197,199,202,204,226,227,228,229,230,],[42,163,169,-113,-113,-94,-113,-113,-112,-81,-82,-83,-84,-85,-86,-90,-93,-96,-101,-103,-106,-108,-89,-95,-102,-107,-111,]),'LCURLY':([40,51,149,181,221,222,253,],[43,58,184,-113,239,-75,256,]),'COMMA':([41,45,46,47,53,54,55,131,137,138,139,140,142,150,151,152,153,154,156,157,158,159,160,161,185,187,190,192,197,199,202,204,211,226,227,228,229,230,242,],[46,46,-113,-23,46,-23,-22,166,172,173,174,175,177,-113,-113,-94,-113,-113,-112,-81,-82,-83,-84,-85,-86,-90,-93,-96,-101,-103,-106,-108,232,-89,-95,-102,-107,-111,232,]),'ASSIGN':([81,101,103,163,],[-113,128,-50,-49,]),'NOTEQUALS':([100,102,104,110,112,113,114,115,116,121,122,128,151,153,154,155,156,157,158,159,160,161,166,172,173,174,175,186,188,189,197,199,202,204,228,229,230,],[126,126,126,126,126,126,126,126,126,126,126,126,196,-113,-113,126,-112,-81,-82,-83,-84,-85,126,126,126,126,126,126,-91,-92,-101,-103,-106,-108,-102,-107,-111,]),'NOT':([100,102,104,110,112,113,114,115,116,121,122,125,126,127,128,155,166,172,173,174,175,186,188,189,],[-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,152,-87,-88,-113,-113,-113,-113,-113,-113,-113,-113,-91,-92,]),'CTE_FLOAT':([100,102,104,110,112,113,114,115,116,121,122,125,126,127,128,155,166,172,173,174,175,186,188,189,191,193,194,195,196,198,200,201,203,205,206,],[-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,159,-87,-88,-113,-113,-113,-113,-113,-113,-113,-113,-91,-92,159,-97,-98,-99,-100,159,-104,-105,159,-109,-110,]),'CTE_CHAR':([100,102,104,110,112,113,114,115,116,118,121,122,125,126,127,128,155,166,172,173,174,175,186,188,189,191,193,194,195,196,198,200,201,203,205,206,],[-113,-113,-113,-113,-113,-113,-113,-113,-113,143,-113,-113,160,-87,-88,-113,-113,-113,-113,-113,-113,-113,-113,-91,-92,160,-97,-98,-99,-100,160,-104,-105,160,-109,-110,]),'CTE_BOOL':([100,102,104,110,112,113,114,115,116,121,122,125,126,127,128,155,166,172,173,174,175,186,188,189,191,193,194,195,196,198,200,201,203,205,206,],[-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,-113,161,-87,-88,-113,-113,-113,-113,-113,-113,-113,-113,-91,-92,161,-97,-98,-99,-100,161,-104,-105,161,-109,-110,]),'AND':([150,151,152,153,154,156,157,158,159,160,161,190,192,197,199,202,204,227,228,229,230,],[188,-113,-94,-113,-113,-112,-81,-82,-83,-84,-85,-93,-96,-101,-103,-106,-108,-95,-102,-107,-111,]),'OR':([150,151,152,153,154,156,157,158,159,160,161,190,192,197,199,202,204,227,228,229,230,],[189,-113,-94,-113,-113,-112,-81,-82,-83,-84,-85,-93,-96,-101,-103,-106,-108,-95,-102,-107,-111,]),'LESSTHAN':([151,153,154,156,157,158,159,160,161,197,199,202,204,228,229,230,],[193,-113,-113,-112,-81,-82,-83,-84,-85,-101,-103,-106,-108,-102,-107,-111,]),'GREATERTHAN':([151,153,154,156,157,158,159,160,161,197,199,202,204,228,229,230,],[194,-113,-113,-112,-81,-82,-83,-84,-85,-101,-103,-106,-108,-102,-107,-111,]),'EQUALS':([151,153,154,156,157,158,159,160,161,197,199,202,204,228,229,230,],[195,-113,-113,-112,-81,-82,-83,-84,-85,-101,-103,-106,-108,-102,-107,-111,]),'PLUS':([153,154,156,157,158,159,160,161,202,204,229,230,],[200,-113,-112,-81,-82,-83,-84,-85,-106,-108,-107,-111,]),'MINUS':([153,154,156,157,158,159,160,161,202,204,229,230,],[201,-113,-112,-81,-82,-83,-84,-85,-106,-108,-107,-111,]),'MULTI':([154,156,157,158,159,160,161,230,],[205,-112,-81,-82,-83,-84,-85,-111,]),'DIVI':([154,156,157,158,159,160,161,230,],[206,-112,-81,-82,-83,-84,-85,-111,]),'ELSE':([249,260,],[253,253,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'gotomain':([2,],[3,]),'empty':([2,3,5,6,9,11,20,24,41,43,45,46,50,53,56,58,61,81,97,100,102,104,110,112,113,114,115,116,121,122,123,128,131,150,151,153,154,155,166,172,173,174,175,181,184,186,211,224,239,242,256,257,],[4,7,10,13,21,13,21,33,47,13,47,54,57,47,62,13,62,103,57,127,127,127,127,127,127,127,127,127,127,127,62,127,167,187,192,199,204,127,127,127,127,127,127,222,225,127,233,62,62,233,258,62,]),'globalvarsblock':([3,],[5,]),'vars':([3,6,11,43,58,],[6,11,11,11,11,]),'globalstartingquad':([5,],[9,]),'varsblock':([6,11,43,58,],[12,23,50,97,]),'type':([8,22,41,45,46,53,],[14,29,48,48,48,48,]),'funcsblock':([9,20,],[19,27,]),'funcs':([9,20,],[20,20,]),'main':([19,],[25,]),'choosetype':([22,],[28,]),'vars1':([24,],[31,]),'paramsblock':([41,45,53,],[44,52,59,]),'params':([41,45,46,53,],[45,45,53,45,]),'main2':([43,],[49,]),'startingquad':([50,97,],[56,123,]),'block':([56,61,123,224,239,257,],[60,99,148,240,248,259,]),'statute':([56,61,123,224,239,257,],[61,61,61,61,61,61,]),'cond':([56,61,123,224,239,257,],[63,63,63,63,63,63,]),'assign':([56,61,123,224,239,257,],[64,64,64,64,64,64,]),'call':([56,61,123,224,239,257,],[65,65,65,65,65,65,]),'cin':([56,61,123,224,239,257,],[66,66,66,66,66,66,]),'cout':([56,61,123,224,239,257,],[67,67,67,67,67,67,]),'delay':([56,61,123,224,239,257,],[68,68,68,68,68,68,]),'forward':([56,61,123,224,239,257,],[69,69,69,69,69,69,]),'backward':([56,61,123,224,239,257,],[70,70,70,70,70,70,]),'turnleft':([56,61,123,224,239,257,],[71,71,71,71,71,71,]),'turnright':([56,61,123,224,239,257,],[72,72,72,72,72,72,]),'servo':([56,61,123,224,239,257,],[73,73,73,73,73,73,]),'lights':([56,61,123,224,239,257,],[74,74,74,74,74,74,]),'display':([56,61,123,224,239,257,],[75,75,75,75,75,75,]),'distance':([56,61,123,224,239,257,],[76,76,76,76,76,76,]),'stop':([56,61,123,224,239,257,],[77,77,77,77,77,77,]),'while':([56,61,123,224,239,257,],[78,78,78,78,78,78,]),'return':([56,61,123,224,239,257,],[79,79,79,79,79,79,]),'era':([56,61,123,224,239,257,],[82,82,82,82,82,82,]),'assign1':([81,],[101,]),'cin1':([83,],[105,]),'cin2':([83,],[106,]),'cin3':([83,],[107,]),'express':([100,102,104,110,112,113,114,115,116,121,122,128,155,166,172,173,174,175,186,],[124,129,131,135,137,138,139,140,141,146,147,162,207,131,214,215,216,217,226,]),'express1':([100,102,104,110,112,113,114,115,116,121,122,128,155,166,172,173,174,175,186,],[125,125,125,125,125,125,125,125,125,125,125,125,125,125,125,125,125,125,125,]),'paramcall':([104,166,],[130,210,]),'relational':([125,],[150,]),'exp':([125,191,198,],[151,227,228,]),'term':([125,191,198,203,],[153,153,153,229,]),'factor':([125,191,198,203,],[154,154,154,154,]),'constant':([125,191,198,203,],[156,156,156,156,]),'paramcall1':([131,],[165,]),'express2':([150,],[185,]),'andor':([150,],[186,]),'relational1':([151,],[190,]),'compare':([151,],[191,]),'exp1':([153,],[197,]),'plusminus':([153,],[198,]),'term1':([154,],[202,]),'multidivi':([154,],[203,]),'while1':([181,],[221,]),'gotoif':([184,],[224,]),'cin4':([211,242,],[231,250,]),'else':([249,260,],[252,261,]),'gotoelse':([256,],[257,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> PROGRAM gotomain globalvarsblock globalstartingquad funcsblock main FIN SEMICOLON','program',8,'p_program','L2C-botMAINPROG.py',239),
  ('gotomain -> empty','gotomain',1,'p_gotomain','L2C-botMAINPROG.py',242),
  ('globalstartingquad -> empty','globalstartingquad',1,'p_globalstartingquad','L2C-botMAINPROG.py',246),
  ('main -> MAIN LPAREN RPAREN LCURLY main2','main',5,'p_main','L2C-botMAINPROG.py',251),
  ('main2 -> varsblock startingquad block RCURLY','main2',4,'p_main2','L2C-botMAINPROG.py',266),
  ('funcsblock -> funcs funcsblock','funcsblock',2,'p_funcsblock','L2C-botMAINPROG.py',270),
  ('funcsblock -> empty','funcsblock',1,'p_funcsblock','L2C-botMAINPROG.py',271),
  ('funcs -> FUNCDEF choosetype ID LPAREN paramsblock RPAREN LCURLY varsblock startingquad block RCURLY','funcs',11,'p_funcs','L2C-botMAINPROG.py',275),
  ('globalvarsblock -> vars varsblock','globalvarsblock',2,'p_globalvarsblock','L2C-botMAINPROG.py',291),
  ('globalvarsblock -> empty','globalvarsblock',1,'p_globalvarsblock','L2C-botMAINPROG.py',292),
  ('varsblock -> vars varsblock','varsblock',2,'p_varsblock','L2C-botMAINPROG.py',306),
  ('varsblock -> empty','varsblock',1,'p_varsblock','L2C-botMAINPROG.py',307),
  ('startingquad -> empty','startingquad',1,'p_startingquad','L2C-botMAINPROG.py',311),
  ('vars -> VARDEF type ID vars1 SEMICOLON','vars',5,'p_vars','L2C-botMAINPROG.py',316),
  ('vars1 -> LBRACKET CTE_INT RBRACKET','vars1',3,'p_vars1','L2C-botMAINPROG.py',326),
  ('vars1 -> empty','vars1',1,'p_vars1','L2C-botMAINPROG.py',327),
  ('choosetype -> type','choosetype',1,'p_choosetype','L2C-botMAINPROG.py',330),
  ('choosetype -> VOID','choosetype',1,'p_choosetype','L2C-botMAINPROG.py',331),
  ('paramsblock -> params paramsblock','paramsblock',2,'p_paramsblock','L2C-botMAINPROG.py',334),
  ('paramsblock -> COMMA params paramsblock','paramsblock',3,'p_paramsblock','L2C-botMAINPROG.py',335),
  ('paramsblock -> empty','paramsblock',1,'p_paramsblock','L2C-botMAINPROG.py',336),
  ('params -> type ID','params',2,'p_params','L2C-botMAINPROG.py',339),
  ('params -> empty','params',1,'p_params','L2C-botMAINPROG.py',340),
  ('block -> statute block','block',2,'p_block','L2C-botMAINPROG.py',355),
  ('block -> empty','block',1,'p_block','L2C-botMAINPROG.py',356),
  ('statute -> cond','statute',1,'p_statute','L2C-botMAINPROG.py',359),
  ('statute -> assign','statute',1,'p_statute','L2C-botMAINPROG.py',360),
  ('statute -> call','statute',1,'p_statute','L2C-botMAINPROG.py',361),
  ('statute -> cin','statute',1,'p_statute','L2C-botMAINPROG.py',362),
  ('statute -> cout','statute',1,'p_statute','L2C-botMAINPROG.py',363),
  ('statute -> delay','statute',1,'p_statute','L2C-botMAINPROG.py',364),
  ('statute -> forward','statute',1,'p_statute','L2C-botMAINPROG.py',365),
  ('statute -> backward','statute',1,'p_statute','L2C-botMAINPROG.py',366),
  ('statute -> turnleft','statute',1,'p_statute','L2C-botMAINPROG.py',367),
  ('statute -> turnright','statute',1,'p_statute','L2C-botMAINPROG.py',368),
  ('statute -> servo','statute',1,'p_statute','L2C-botMAINPROG.py',369),
  ('statute -> lights','statute',1,'p_statute','L2C-botMAINPROG.py',370),
  ('statute -> display','statute',1,'p_statute','L2C-botMAINPROG.py',371),
  ('statute -> distance','statute',1,'p_statute','L2C-botMAINPROG.py',372),
  ('statute -> stop','statute',1,'p_statute','L2C-botMAINPROG.py',373),
  ('statute -> while','statute',1,'p_statute','L2C-botMAINPROG.py',374),
  ('statute -> return','statute',1,'p_statute','L2C-botMAINPROG.py',375),
  ('cond -> IF LPAREN express RPAREN LCURLY gotoif block RCURLY else','cond',9,'p_cond','L2C-botMAINPROG.py',378),
  ('gotoif -> empty','gotoif',1,'p_gotoif','L2C-botMAINPROG.py',384),
  ('else -> ELSE LCURLY gotoelse block RCURLY else','else',6,'p_else','L2C-botMAINPROG.py',393),
  ('else -> SEMICOLON','else',1,'p_else','L2C-botMAINPROG.py',394),
  ('gotoelse -> empty','gotoelse',1,'p_gotoelse','L2C-botMAINPROG.py',397),
  ('assign -> ID assign1 ASSIGN express SEMICOLON','assign',5,'p_assign','L2C-botMAINPROG.py',406),
  ('assign1 -> LBRACKET express RBRACKET','assign1',3,'p_assign1','L2C-botMAINPROG.py',426),
  ('assign1 -> empty','assign1',1,'p_assign1','L2C-botMAINPROG.py',427),
  ('call -> era LPAREN paramcall RPAREN SEMICOLON','call',5,'p_call','L2C-botMAINPROG.py',430),
  ('era -> ID','era',1,'p_era','L2C-botMAINPROG.py',455),
  ('paramcall -> express paramcall1','paramcall',2,'p_paramcall','L2C-botMAINPROG.py',468),
  ('paramcall1 -> COMMA paramcall','paramcall1',2,'p_call1','L2C-botMAINPROG.py',479),
  ('paramcall1 -> empty','paramcall1',1,'p_call1','L2C-botMAINPROG.py',480),
  ('cin -> CIN cin1 SEMICOLON','cin',3,'p_cin','L2C-botMAINPROG.py',484),
  ('cin1 -> cin2','cin1',1,'p_cin1','L2C-botMAINPROG.py',487),
  ('cin1 -> cin3','cin1',1,'p_cin1','L2C-botMAINPROG.py',488),
  ('cin2 -> LPAREN ID RPAREN','cin2',3,'p_cin2','L2C-botMAINPROG.py',491),
  ('cin3 -> LBRACKET CTE_INT RBRACKET LPAREN cin4 RPAREN','cin3',6,'p_cin3','L2C-botMAINPROG.py',494),
  ('cin4 -> COMMA ID cin4','cin4',3,'p_cin4','L2C-botMAINPROG.py',497),
  ('cin4 -> empty','cin4',1,'p_cin4','L2C-botMAINPROG.py',498),
  ('cout -> COUT LPAREN express RPAREN SEMICOLON','cout',5,'p_cout','L2C-botMAINPROG.py',501),
  ('delay -> DELAY LPAREN CTE_INT RPAREN SEMICOLON','delay',5,'p_delay','L2C-botMAINPROG.py',504),
  ('forward -> FORWARD LPAREN express COMMA express RPAREN SEMICOLON','forward',7,'p_forward','L2C-botMAINPROG.py',507),
  ('backward -> BACKWARD LPAREN express COMMA express RPAREN SEMICOLON','backward',7,'p_backward','L2C-botMAINPROG.py',515),
  ('turnleft -> TURNLEFT LPAREN express COMMA express RPAREN SEMICOLON','turnleft',7,'p_turnleft','L2C-botMAINPROG.py',518),
  ('turnright -> TURNRIGHT LPAREN express COMMA express RPAREN SEMICOLON','turnright',7,'p_turnright','L2C-botMAINPROG.py',521),
  ('servo -> SERVO LPAREN express RPAREN SEMICOLON','servo',5,'p_servo','L2C-botMAINPROG.py',524),
  ('lights -> LIGHTS LPAREN CTE_INT COMMA CTE_INT RPAREN SEMICOLON','lights',7,'p_lights','L2C-botMAINPROG.py',527),
  ('display -> DISPLAY LPAREN CTE_CHAR RPAREN SEMICOLON','display',5,'p_display','L2C-botMAINPROG.py',530),
  ('distance -> DISTANCE LPAREN RPAREN SEMICOLON','distance',4,'p_distance','L2C-botMAINPROG.py',533),
  ('stop -> STOP LPAREN RPAREN SEMICOLON','stop',4,'p_stop','L2C-botMAINPROG.py',536),
  ('while -> WHILE LPAREN express RPAREN while1 LCURLY block RCURLY SEMICOLON','while',9,'p_while','L2C-botMAINPROG.py',539),
  ('while1 -> empty','while1',1,'p_while1','L2C-botMAINPROG.py',546),
  ('return -> RETURN LPAREN express RPAREN SEMICOLON','return',5,'p_return','L2C-botMAINPROG.py',555),
  ('type -> INT','type',1,'p_type','L2C-botMAINPROG.py',561),
  ('type -> FLOAT','type',1,'p_type','L2C-botMAINPROG.py',562),
  ('type -> BOOL','type',1,'p_type','L2C-botMAINPROG.py',563),
  ('type -> CHAR','type',1,'p_type','L2C-botMAINPROG.py',564),
  ('constant -> ID','constant',1,'p_constant','L2C-botMAINPROG.py',569),
  ('constant -> CTE_INT','constant',1,'p_constant','L2C-botMAINPROG.py',570),
  ('constant -> CTE_FLOAT','constant',1,'p_constant','L2C-botMAINPROG.py',571),
  ('constant -> CTE_CHAR','constant',1,'p_constant','L2C-botMAINPROG.py',572),
  ('constant -> CTE_BOOL','constant',1,'p_constant','L2C-botMAINPROG.py',573),
  ('express -> express1 relational express2','express',3,'p_express','L2C-botMAINPROG.py',595),
  ('express1 -> NOTEQUALS','express1',1,'p_express1','L2C-botMAINPROG.py',598),
  ('express1 -> empty','express1',1,'p_express1','L2C-botMAINPROG.py',599),
  ('express2 -> andor express','express2',2,'p_express2','L2C-botMAINPROG.py',602),
  ('express2 -> empty','express2',1,'p_express2','L2C-botMAINPROG.py',603),
  ('andor -> AND','andor',1,'p_andor','L2C-botMAINPROG.py',606),
  ('andor -> OR','andor',1,'p_andor','L2C-botMAINPROG.py',607),
  ('relational -> exp relational1','relational',2,'p_relational','L2C-botMAINPROG.py',625),
  ('relational -> NOT','relational',1,'p_relational','L2C-botMAINPROG.py',626),
  ('relational1 -> compare exp','relational1',2,'p_relational1','L2C-botMAINPROG.py',629),
  ('relational1 -> empty','relational1',1,'p_relational1','L2C-botMAINPROG.py',630),
  ('compare -> LESSTHAN','compare',1,'p_compare','L2C-botMAINPROG.py',648),
  ('compare -> GREATERTHAN','compare',1,'p_compare','L2C-botMAINPROG.py',649),
  ('compare -> EQUALS','compare',1,'p_compare','L2C-botMAINPROG.py',650),
  ('compare -> NOTEQUALS','compare',1,'p_compare','L2C-botMAINPROG.py',651),
  ('exp -> term exp1','exp',2,'p_exp','L2C-botMAINPROG.py',655),
  ('exp1 -> plusminus exp','exp1',2,'p_exp1','L2C-botMAINPROG.py',673),
  ('exp1 -> empty','exp1',1,'p_exp1','L2C-botMAINPROG.py',674),
  ('plusminus -> PLUS','plusminus',1,'p_plusminus','L2C-botMAINPROG.py',677),
  ('plusminus -> MINUS','plusminus',1,'p_plusminus','L2C-botMAINPROG.py',678),
  ('term -> factor term1','term',2,'p_term','L2C-botMAINPROG.py',682),
  ('term1 -> multidivi term','term1',2,'p_term1','L2C-botMAINPROG.py',700),
  ('term1 -> empty','term1',1,'p_term1','L2C-botMAINPROG.py',701),
  ('multidivi -> MULTI','multidivi',1,'p_multidivi','L2C-botMAINPROG.py',704),
  ('multidivi -> DIVI','multidivi',1,'p_multidivi','L2C-botMAINPROG.py',705),
  ('factor -> LPAREN express RPAREN','factor',3,'p_factor','L2C-botMAINPROG.py',709),
  ('factor -> constant','factor',1,'p_factor','L2C-botMAINPROG.py',710),
  ('empty -> <empty>','empty',0,'p_empty','L2C-botMAINPROG.py',713),
]
