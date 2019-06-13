'''
Created on Jun 5, 2014

@author: Alejandro
'''

import sys

from ClaseSimulador import Simulador

def main():
	
	# Manejo de argumentos
	if len(sys.argv) != 3:
		print('Numero de argumentos incorrecto. \n' \
			  'Uso: python3 simulador.py <arch_entrada> <arch_salida>')
		sys.exit(-1)
	
	# Manejo de archivo de salida
	try:
		salidaEstandar = sys.stdout
		oFile = open(sys.argv[2], 'w')
		sys.stdout = oFile	# Redirige los prints al archivo de salida
	except IOError:
		raise IOError('Error al abrir el archivo de salida')
	
	# Crear un simulador
	simulador = Simulador(sys.argv[1])
	simulador.simular()
	
	# Cerrar el archivo de salida y regresar la salida estandar
	# a su estado original
	sys.stdout = salidaEstandar
	oFile.close()

if __name__ == '__main__':
	main()