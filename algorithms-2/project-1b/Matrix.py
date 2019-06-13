#!/usr/bin/env python

"""
	Archivo: Matrix.py
	
	Descripcion: implementacion del TAD Matriz. Permite operaciones
	basicas en matrices con numeros enteros
	
	@author: Manuel Pacheco
	@contact: manuelalejandropm@gmail.com
	
	Fecha de ultima modificacion: Abril 2014
"""

class Matrix:
	
	def __init__(self,row=0,column=0):
		"""
		@summary: Construye la estructura de datos bidimensional necesaria
		para la matriz.
		
		@param row: numero de filas de la matriz.
		@type row: int
		
		@param column: numero de columnas de la matriz.
		@type column: int
		
		@status: testing
		@version: 1
		
		@precondition: row y columna deben ser positivos.
		
		@raise ValueError: cuando row o column son no-positivos.
		
		@return: la nueva referencia a objeto Matrix.
		@rtype: Matrix
		"""
		
		
		if row <= 0 or column <= 0:
			raise ValueError("Los tamaños de la matriz (" + row + "x"
				+ column + ") deben ser positivos.")
		self.data = [[0 for j in range(column)] for j in range(row)]  # @UnusedVariable
	
	@staticmethod
	def crear(row, column):
		"""
		@summary: metodo publico para crear la matriz.
		
		@param row: numero de filas de la matriz.
		@type row: int
		
		@param column: numero de columnas de la matriz.
		@type column: int
		
		@status: testing
		@version: 1
		
		@return: la nueva referencia a objeto Matrix.
		@rtype: Matrix
		"""
		
		return Matrix()
	
	def get(self, row, column):
		"""
		@summary: metodo publico para obtener una casilla.
		
		@param row: fila de la matriz.
		@type row: int
		
		@param column: columna de la matriz.
		@type column: int
		
		@status: testing
		@version: 1
		
		@precondition: row y columna deben ser positivos.
		
		@raise ValueError: cuando row o column son no-positivos.
		
		@return: el valor dentro de la casilla.
		@rtype: indefinido. Generalmente un entero.
		"""
		if (row >= len(self.data) or row < 0 or
				column >= len(self.data[0]) or column < 0):
			raise ValueError("Los datos (" + str(row) + "," + str(column) + 
							") insertados deben ser positivos y en el rango.")
		
		return self.data[row][column]
	
	def set(self, row, column, value):
		"""
		@summary: metodo publico para cambiar el contenido de una casilla.
		
		@param row: fila de la matriz.
		@type row: int
		
		@param column: columna de la matriz.
		@type column: int
		
		@note: la clase soporta la insercion de cualquier tipo de objeto. Es
		responsabilidad del usuario de la clase que esta flexibilidad no le
		genere conflictos.
		
		@status: testing
		@version: 1
		
		@return: la matrix modificada
		@rtype: Matrix
		"""
		self.data[row][column] = value
		return self
	
	def add(self, mat):
		"""
		@summary: suma la matriz interna a la matriz 'mat' y retorna el
		resultado. No modifica ninguna de las matrices argumento.
		
		@param mat: segunda matriz operando de la suma
		@type: Matrix
		
		@status: testing
		@version: 1
		
		@precondition: las matrices son de igual tamaño
		
		@raise ValueError: si las matrices no son de igual tamaños
		
		@return: matriz resultado de la suma
		@rtype: Matrix
		"""
		if (len(self.data) != len(mat.data) or
				len(self.data[0]) != len(mat.data[0])):
			pass
			raise ValueError("Las matrices no son de igual tamaño.")
		
		out = Matrix(len(self.data), len(self.data[0]))
		
		for i in range(len(self.data)):
			for j in range(len(self.data[0])):
				out.data[i][j] = self.data[i][j] + mat.data[i][j]
		
		return out
	
	def mult_scalar(self, scalar):
		"""
		@summary: multiplica la matriz interna por un valor escalar
		
		@param scalar: escalar de la multiplicacion
		@type: double/integer
		
		@status: testing
		@version: 1
		
		@raise TypeError: si scalar no es un valor numerico
		
		@return: matriz resultado de la multiplicacion escalar
		@rtype: Matrix
		"""
		out = Matrix(len(self.data), len(self.data[0]))
		
		for i, row in enumerate(self.data):
			for j, elem in enumerate(row):
				out.data[i][j] = elem * scalar
		
		return out
	
	def transpose(self):
		"""
		@summary: obtiene la matriz traspuesta de la matriz interna
		
		@status: testing
		@version: 1
		
		@return: matriz transpuesta
		@rtype: Matrix
		"""
		out = Matrix(len(self.data[0]), len(self.data))
		
		for i, row in enumerate(self.data):
			for j, elem in enumerate(row):
				out.data[j][i] = elem
		
		return out
	
	def multiply(self,mat):
		"""
		@summary: multiplica la matriz interna por una matriz dada. No modifica
		ninguna de las matrices argumento.
		
		@status: testing
		@version: 1
		
		@raise ValueError: si las matrices no tienen tamaños compatibles
		
		@return: matriz resultante de la multiplicacion
		@rtype: Matrix
		"""
		
		if len(self.data[0]) != len(mat.data):
			raise ValueError("Las tamaños de las matrices no son compatibles" +
							"para la multiplicacion.")
		
		out = Matrix(len(self.data), len(mat.data[0]))
		
		for i in range(len(self.data)):
			for j in range(len(mat.data[0])):
				for k in range(len(mat.data)):
					out.data[i][j] += self.data[i][k] * mat.data[k][j]
		
		return out