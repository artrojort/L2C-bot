# L2C-bot
---
##Proyecto de diseño de compiladores
Este documento compone el Manual de Usuario del proyecto L2C-Bot.
Autores:
  * Arturo Rojas Ortiz
  * Diego Jimenez Torres 

##Carro
El carro debe de estar conectado en un puero USB de la computadora que se encargará de realizar la programacion.
El switch del vehiculo debe estar en la posicion de encendido.

##Lenguaje
El archivo del codigo tiene que estar dentro de la carpeta `docs > tests`.
El archivo del codigo tiene que tener una terminacion `.txt`.

##Posibles Errores 
###Puerto (Socket)
De los errores más comunes, se encuentran la asignación del puerto USB de la conexión al microcontrolador de Arduino. Ës sumamente importante colocarle a la máquina virtual el valor del socket correcto, de lo contrario el programa no ejecuta. Donde tiene que especificarse el puerto correcto, dependiendo del sistema operativo. En el caso de Windows se utiliza comúnmente el puerto **‘COM3’** y en el caso de linux en su distro de Ubuntu utiliza comúnmente el puerto **‘/dev/ttyACM0’**. 

Para revisar el puerto que el sistema operativo le asignó al microcontrolador es necesario abrir el Arduino IDE y revisar dentro de la sección de Tools > Port, el puerto que aparece a seleccionar.

###Llamada de función con 2 o más funciones como argumento de parámetro.
Cuando se requiere utilizar una función que recibe como parámetro, el resultado de 2 o más funciones, el valor de retorno es inesperado. Los quads que se generan a partir de ese tipo de función, se generan erróneamente. De manera que es difícil encontrar en qué sección es en donde está ocurriendo el problema. Ya sea por que se esta yendo por algún camino distinto, o que el orden en el cual se generan pueden variar, por tanto se optó por reorganizar su estructura, y solo limitar a poder utilizar solamente una función como parámetro de otra.
