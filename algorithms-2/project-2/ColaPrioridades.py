'''
Created on Jun 4, 2014

@author: Manuel Pacheco
'''

class ColaPrioridades:
	'''Implementa una cola de prioridades basica'''

	def __init__(self,comparador=None):
		'''Constructor. Permite asignar una funcion para evaluar
		los elementos.'''
		self.objetos = []				# Elementos en la cola
		if comparador == None:
			self.comparador = ColaPrioridades.comparadorDefecto
		else:
			self.comparador = comparador	# Funcion para comparar elementos
	
	def agregar(self,obj):
		'''Agrega un elemento a la cola de prioridades'''
		# Compara si obj debe ir antes del elemento comparado
		for (indice, objeto) in enumerate(self.objetos):
			if self.comparador(obj,objeto) > 0:
				self.objetos.insert(indice, obj)
				return
		# En caso de que sea el ultimo
		self.objetos.append(obj)
	
	def __len__(self):
		'''Retorna el numero de elementos en la cola'''
		return len(self.objetos)
	
	def obtener(self,i):
		'''Retorna el elemento i de la cola'''
		return self.objetos[i]
	
	def remover(self,i):
		'''Remueve el elemento i de la cola'''
		del(self.objetos[i])
	
	def obtElementos(self):
		return self.objetos
		
	@staticmethod
	def comparadorDefecto(obj1,obj2):
		'''Retorna positivo, cero, o negativo, dependiendo de comparar los
		objetos como si fueran enteros'''
		return obj1-obj2
	
	def imprimirEstado(self):
		'''Imprime el estado de los atributos del objeto'''
		print('IMPRESION DE COLA:')
		for (indice, objeto) in enumerate(self.objetos):
			print('ELEMENTO %d:' % indice)
			try:
				objeto.imprimirEstado()
			except AttributeError:
				print(objeto)
