#code to read potenciometro
import pyfirmata
import time

#MAKE SURE ttyACM* is on the correct place

#ls -l /dev/ttyACM*

board = pyfirmata.Arduino('/dev/ttyACM1')

it = pyfirmata.util.Iterator(board)
it.start()

#analog pin A0  (analog:#0:input)
analog_input = board.get_pin('a:0:i')

while True:
    analog_value = analog_input.read()
    print(analog_value)
    time.sleep(0.2)