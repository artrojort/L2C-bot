#Code to test the Ultrasonic Sensor
import pyfirmata
import time

board = pyfirmata.Arduino('/dev/ttyACM0')

it = pyfirmata.util.Iterator(board)
it.start()

led = board.get_pin("d:4:o")
trigger = board.get_pin("d:9:o")
echo = board.get_pin("d:10:i")



while True:
	# Make sure trigger is off at loop start
	trigger.write(0)
	time.sleep(1)
	
	#Emit ultrasonic sound wave
	trigger.write(1)
	
	time.sleep(5)
	trigger.write(0)


	duration = PULSE_IN(echo.read())
	distance = duration * 0.34 / 2

	print(distance)


