#code to do full checkup for the working car
import pyfirmata
import time

#MAKE SURE ttyACM* is on the correct place

#ls -l /dev/ttyACM*

board = pyfirmata.Arduino('/dev/ttyACM0')

it = pyfirmata.util.Iterator(board)
it.start()

#analog pin A0  (analog:#0:input)
analog_input = board.get_pin('a:0:i')

#digital pin
# 13 motorA
# 12 motorA
# 11 enA
# 10 enB
# 9 motorB
# 8 motorB
in4 = board.get_pin('d:13:o')
in3 = board.get_pin('d:12:o')
enA = board.get_pin('d:11:o')
enB = board.get_pin('d:10:o')
in2 = board.get_pin('d:9:o')
in1 = board.get_pin('d:8:o')


# 7 led Gree
# 6 led Yellow
# 5 led Red
# 4 led Blue - Escritura
ledG = board.get_pin('d:7:o')
ledY = board.get_pin('d:6:o')
ledR = board.get_pin('d:5:o')
ledB = board.get_pin('d:4:o')

#servo  (digitalPWM:#3:servo)
servoP = board.get_pin('d:3:s')

def display():
	ledB.write(1)
	#Asegurarse de que imprima la cadena de string en la consola
	print()
	time.sleep(3)
	ledB.write(0)

def delay(seconds):
	time.sleep(seconds)

def forward(seconds):
	in1.write(1)
	in4.write(1)
	time.sleep(seconds)
	in1.write(0)
	in4.write(0)

def backward(seconds):
	in2.write(1)
	in3.write(1)
	time.sleep(seconds)
	in2.write(0)
	in3.write(0)

def turnLeft(seconds):
	in2.write(1)
	in4.write(1)
	time.sleep(seconds)
	in2.write(0)
	in4.write(0)

def turnRight(seconds):
	in1.write(1)
	in3.write(1)
	time.sleep(seconds)
	in1.write(0)
	in3.write(0)

def servo(degree):
	#Asegurarse que "degree" este en un rango 0-180
	servoP.write(degree)

def lights(option):
	#Asegurarse que "option" sea [apagado/prendido/parpadeando]
	if option == "prendido":
		ledR.write(1)
		ledY.write(1)
		ledG.write(1)

	if option == "apagado":
		ledR.write(0)
		ledY.write(0)
		ledG.write(0)

	if option == "parpadeando":
		vueltas = 0

		while vueltas < 3:
			ledR.write(1)
			ledY.write(1)
			ledG.write(1)
			time.sleep(1)
			ledR.write(0)
			ledY.write(0)
			ledG.write(0)
			vueltas = vueltas + 1






