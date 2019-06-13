#!/usr/bin/env python

"""
	Archivo: matrix_calc.py
	
	Descripcion: Interpreta operaciones a realizar sobre matrices utilizando
	la clase Matrix
	
	@author: Manuel Pacheco
	@contact: manuelalejandropm@gmail.com
	
	Fecha de ultima modificacion: Abril 2014
"""

from Matrix import Matrix
import sys

def processCommand(mList,line):
	"""
	@summary: Parsea una linea de comandos y encadena los comandos necesarios para
	procesarlos
	
	@param mList: lista de matrices utilizadas en los procesos
	@type mList: list(Matrix())
	
	@status: testing
	@version: 1
	
	@raise IOException: si el archivo no se consigue
	
	@param linea: linea que posee el comando y sus argumentos
	"""
	
	# Separar las partes de la linea
	sections = line.split(None,1)
	cmd = sections[0]
	args = sections[1].split()

	if cmd == "READ":
		fileName = args[0]
		mNum = parseNum(args[1])
		
		try:
			iFile =  open(fileName)
			params = iFile.readline()
			(row, col) = [int(s) for s in params.split()]
			
			out = Matrix(row,col)
			
			for i in range(row):
				rowElems = iFile.readline().split()
				for j in range(col):
					out.data[i][j] = int(rowElems[j])
		
		except IOError:
			print("IOERROR READ")
			sys.exit(-1)
		
		mList[mNum] = out
		return out
	
	if cmd == "NEW":
		row = int(args[0])
		col = int(args[1])
		mNum = parseNum(args[2])
		
		out = Matrix(row,col)
		mList[mNum] = out
		return out
	
	if cmd == "GET":
		mNum = parseNum(args[0])
		row = int(args[1])
		col = int(args[2])
		
		return mList[mNum].get(row,col)
	
	if cmd == "SET":
		mNum = parseNum(args[0])
		row = int(args[1])
		col = int(args[2])
		val = int(args[3])
		
		mList[mNum].set(row,col,val)
		return mList[mNum]
	
	if cmd == "ADD":
		mNum0 = parseNum(args[0])
		mNum1 = parseNum(args[1])
		mNum2 = parseNum(args[2])
	
		out = mList[mNum0].add(mList[mNum1])
		mList[mNum2] = out
		return out
	
	if cmd == "SCALAR":
		mNum0 = parseNum(args[0])
		scalar = int(args[1])
		mNum1 = parseNum(args[2])
		
		out = mList[mNum0].mult_scalar(scalar)
		mList[mNum1] = out
		return out
	
	if cmd == "TRANSPOSE":
		mNum0 = parseNum(args[0])
		mNum1 = parseNum(args[1])
		
		out = mList[mNum0].transpose()
		mList[mNum1] = out
		return out
	
	if cmd == "MULTIPLY":
		mNum0 = parseNum(args[0])
		mNum1 = parseNum(args[1])
		mNum2 = parseNum(args[2])
	
		out = mList[mNum0].multiply(mList[mNum1])
		mList[mNum2] = out
		return out
	
def parseNum(string):
	"""Devuelve un valor entero a partir de un string como M0...M9"""
	if not string.startswith("M"):
		raise ValueError("Nombre de matriz no empieza por 'M'.")
	
	out = int(string[1:])
	
	if out < 0 or out > 9:
		raise ValueError("El numero de la matriz no es valido.")
	
	return out

def getSize(num):
	if num < 0:
		num = -num
	
	size = 0;
	while (True):
		num = num // 10
		size += 1
		
		if num == 0:
			break
	
	return size

def getMaxWidth(mat):
	size = 1;
	for row in mat.data:
		for elem in row:
			new = getSize(elem)
			if new > size:
				size = new
	return size

def formatText(text, size):
	if size <= len(text): return text
	else: return (" " * (size - len(text))) + text

def processOutput(obj):
	if isinstance(obj,int):
		return str(obj)
	
	elif isinstance(obj,Matrix):
		width = getMaxWidth(obj)
		dashes = "-" * ((width + 1) * len(obj.data[0]) - 1)
		out = ""
		
		out += dashes + "\n"
		
		for row in obj.data:
			for j, elem in enumerate(row):
				out += formatText(str(elem),width)
				
				if j+1 != len(row):
					out += " "
			
			out += "\n"
		
		out += dashes
		return out

def main():
	"""Codigo primario de ejecucion del programa"""

	## Manejo de argumentos
	if len(sys.argv) != 3:
		print("Numero de argumentos incorrecto. \n" \
			  "Uso: python3 matrix_calc.py <arch_instrucciones> <arch_salida>")
		sys.exit(-1)
	
	## Manejo de archivos
	try:
		iFile = open(sys.argv[1], 'r')
		oFile = open(sys.argv[2], 'w')
	except IOError:
		print("Error al abrir los archivos. Saliendo...")
		sys.exit(-1)
	
	## Inicializacion
	mList = [None] * 10
	
	## Letura de Instrucciones
	try:
		for line in iFile:
			output = processCommand(mList,line)
			oFile.write(processOutput(output))
			oFile.write("\n")
	
	except IOError:
		print("Error al trabajar en los archivos. Saliendo...")
		sys.exit(-1)
	
if __name__ == "__main__":
	main()
