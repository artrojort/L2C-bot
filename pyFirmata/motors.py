#Code to test motors
import pyfirmata
import time

board = pyfirmata.Arduino('/dev/ttyACM3')

it = pyfirmata.util.Iterator(board)
it.start()

# pins Motor A
# 13- positive
# 12- negative
# 11- ~PWM

# pins Motor B
# 8- positive
# 9- negative
# 10- ~PWM
i = 1
x = 0

enB = board.get_pin('d:11:p')
in3 = board.get_pin('d:12:o')
in4 = board.get_pin('d:13:o')

while i < 6 :
	in3.write(1)
	in4.write(0)
	print("forward 3")
	time.sleep(.5)
	
	in3.write(0)
	in4.write(0)
	print("stop 1")
	time.sleep(1)

	in3.write(1)
	in4.write(0)
	print("forward 2")
	time.sleep(.5)

	in3.write(0)
	in4.write(0)
	print("stop 1")
	time.sleep(1)
	
	in3.write(1)
	in4.write(0)
	print("forward 1")
	time.sleep(1)

	in3.write(0)
	in4.write(0)
	print("stop 1")
	time.sleep(1)
	

	# while x < 1 :
	# 	enB.write(x)
	# 	x = x + .2 
	# 	# print(x)

	# enB.write(0)
	# time.sleep(1)

	# enB.write(0.5)
	# time.sleep(1)

	# enB.write(1)
	# time.sleep(1)	

	# board.digital[13].write(0)
	# board.digital[12].write(1)
	# time.sleep(1)
	# print("backward")
	x=0
	i=i+1
	print(i)


board.digital[13].write(0)
board.digital[12].write(0)