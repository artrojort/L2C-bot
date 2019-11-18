#code to operate servo using potenciometro
import pyfirmata
import time

#MAKE SURE ttyACM* is on the correct place

#ls -l /dev/ttyACM*

board = pyfirmata.Arduino('/dev/ttyACM3')

it = pyfirmata.util.Iterator(board)
it.start()

#analog pin A0  (analog:#0:input)
analog_input = board.get_pin('a:0:i')
#returns float value

#servo declaration (digitalPWM:#3:servo)
servo = board.get_pin('d:3:s')

#servo only allows values in degrees from 0-360
def pwToSer(n):
	if n == None:
		x=0
	else:
	    x = 180 * n
	return int(x)


while True:
    analog_value = analog_input.read()
    servo_value = pwToSer(analog_value)
   # servo.write(0)
    print(str(analog_value)+" - "+str(servo_value))

    #time.sleep(1)
    servo.write(servo_value)
    time.sleep(0.1)