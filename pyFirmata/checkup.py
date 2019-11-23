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
servo = board.get_pin('d:3:s')


#servo only allows values in degrees from 0-180
def pwToSer(n):
	if n == None:
		x=0
	else:
	    x = 180 * n
	return int(x)




# Start all motors on off
in4.write(0)
in3.write(0)
in2.write(0)
in1.write(0)

print("Moving forward")
#move forward
in1.write(1)
in4.write(1)
time.sleep(3)
in1.write(0)
in4.write(0)

print("Moving backward")
#move backward
in2.write(1)
in3.write(1)
time.sleep(3)
in2.write(0)
in3.write(0)

time.sleep(3)

#turn on leds one by one
print("Green Led")
ledG.write(1)
time.sleep(1)

print("Yellow Led")
ledG.write(0)
ledY.write(1)
time.sleep(1)

print("Red Led")
ledY.write(0)
ledR.write(1)
time.sleep(1)

print("Blue Led")
ledR.write(0)
ledB.write(1)
time.sleep(1)

ledB.write(0)
            # To test Potenciometro USAGE
           #  #Test servo
           #  analog_value = analog_input.read()
           #  servo_value = pwToSer(analog_value)
           # # servo.write(0)
           #  print(str(analog_value)+" - "+str(servo_value))
           # END

time.sleep(1)
print("Servo 0 degrees")
servo.write(1)
time.sleep(1)

print("Servo 180 degrees")
servo.write(180)
time.sleep(1)

print("Checkup succesfull!")