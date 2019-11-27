# L2C-bot
## Proyecto de diseño de compiladores
Este documento compone el Manual de Usuario del proyecto L2C-Bot.  
Autores:
  * Arturo Rojas Ortiz
  * Diego Jimenez Torres 

## Como utilizar L2C-Bot
1. Elaborar un archivo de texto siguiendo la sintaxis 

### Funciones Propias del Carro

#### cin()
> Lee una o más entradas desde consola

#### cout()
> Imprime un tipo de dato int/float/boo/char

#### delay(segundos)
> Genera una interrupción medida en segundos durante la ejecución de las tareas del carro.

#### forward(segundos)
> Mueve hacia adelante al carro durante cierto tiempo en segundos.

#### backward(segundos)
> Mueve hacia atrás al carro durante cierto tiempo en segundos.

#### turnleft(segundos)
> El carro gira sobre su eje hacia la izquierda durante cierto tiempo en segundos.

#### turnright(segundos)
> El carro gira sobre su eje hacia la derecha durante cierto tiempo en segundos.

#### servo(grados)
> Mueve al servomotor del carro que controla una pala a una posicion entre 0 y 180 grados.

#### lights(int,int)
> Se le indica el numero de led y un estatus.  
> El numero de led toma un valor entre 1, 2 y 3, donde se elige (1 : para led rojo, 2 : para led amarillo, 3 : para led verde)  
> El estatus también se elige de una constante entre 1, 2 y 3, donde se elige (1 : prendido, 2 : apagado, 3 : parpadeando)


## Advertencias
### Carro
El carro debe de estar conectado en un puero USB al momento de ejecutar el codigo.  
El switch del vehiculo debe estar en la posicion de encendido.

### Lenguaje
El archivo del codigo tiene que estar dentro de la carpeta `docs > tests`.  
El archivo del codigo tiene que tener una terminacion `.txt`.

## Posibles Errores 
### Puerto (Socket)
De los errores más comunes, se encuentran la asignación del puerto USB de la conexión al microcontrolador de Arduino. Es sumamente importante colocarle a la máquina virtual el valor del socket correcto, de lo contrario el programa no ejecuta. Donde tiene que especificarse el puerto correcto, dependiendo del sistema operativo. En el caso de Windows se utiliza comúnmente el puerto **‘COM3’** y en el caso de linux en su distro de Ubuntu utiliza comúnmente el puerto **‘/dev/ttyACM0’**. 

Para revisar el puerto que el sistema operativo le asignó al microcontrolador es necesario abrir el Arduino IDE y revisar dentro de la sección de Tools > Port, el puerto que aparece a seleccionar.

### Llamada de función con 2 o más funciones como argumento de parámetro.
Cuando se requiere utilizar una función que recibe como parámetro, el resultado de 2 o más funciones, el valor de retorno es inesperado. Los quads que se generan a partir de ese tipo de función, se generan erróneamente. De manera que es difícil encontrar en qué sección es en donde está ocurriendo el problema. Ya sea por que se esta yendo por algún camino distinto, o que el orden en el cual se generan pueden variar, por tanto se optó por reorganizar su estructura, y solo limitar a poder utilizar solamente una función como parámetro de otra.
