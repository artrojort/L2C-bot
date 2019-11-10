#Code to prove Firmata functionality on Arduino microcontroler.
import pyfirmata
import time

board = pyfirmata.Arduino('/dev/ttyACM0')

while True:
    board.digital[4].write(1)
    time.sleep(1)
    board.digital[4].write(0)
    time.sleep(1)