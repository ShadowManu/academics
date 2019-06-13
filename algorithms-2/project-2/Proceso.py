'''
Created on Jun 4, 2014

@author: Manuel Pacheco
'''

class Proceso:
	'''Implementa las propiedades de un proceso virtual'''
	minPrioridad = 1	# Prioridad minima con la que se puede crear un proceso
	maxPrioridad = 5	# Prioridad maxima con la que se puede crear un proceso
	STR_PROC_INICIAR = '%d Iniciando proceso %s en el procesador %d.'
	STR_PROC_TERMINAR = '%d Finalizando proceso %s en el procesador %d.'

	def __init__(self,nombre,prioridad,memoria,duracion,subprocesos,procesador=None):
		'''
		Constructor
		'''
		# Atributos principales
		self.nombre = str(nombre)
		self.prioridad = int(prioridad)
		self.memoria = int(memoria)
		self.duracion = int(duracion)
		self.subprocesos = int(subprocesos)
		
		# Atributos auxiliares
		self.restante = self.duracion
		self.terminado = False
		self.procesador = procesador
		
		# Validacion de argumentos
		if (self.prioridad < Proceso.minPrioridad or
			self.prioridad > Proceso.maxPrioridad):
			raise ValueError('La prioridad tiene un valor invalido')
		if (self.memoria < 0):
			raise ValueError('La memoria debe ser positiva o cero')
		if (self.duracion <= 0):
			raise ValueError('La duracion debe ser positiva')
		if (self.subprocesos < 0):
			raise ValueError('Los subprocesos deben ser positivos o cero')
	
	def obtNombre(self):
		return self.nombre
	
	def obtPrioridad(self):
		return self.prioridad
	
	def obtMemoria(self):
		return self.memoria
	
	def obtDuracion(self):
		return self.duracion
	
	def obtSubprocesos(self):
		return self.subprocesos
	
	def obtProcesadorAsociado(self):
		return self.procesador
	
	def enlazarProcesador(self,procesador):
		self.procesador = procesador
	
	def simularTiempo(self):
		# Si el proceso inicia
		if self.duracion == self.restante:
			# Impresion de inicio
			print(self.STR_PROC_INICIAR % (
					self.obtProcesadorAsociado() \
					.obtSimuladorAsociado().obtTiempo(),	# Tiempo
					self.obtNombre(),						# Nombre
					self.obtProcesadorAsociado().obtId()))	# Procesador
		
		self.restante -= 1
		
		if self.restante == 0:
			# Impresion de inicio
			print(self.STR_PROC_TERMINAR % (
					self.obtProcesadorAsociado() \
					.obtSimuladorAsociado().obtTiempo(),	# Tiempo
					self.obtNombre(),						# Nombre
					self.obtProcesadorAsociado().obtId()))	# Procesador
			
			self.terminado = True
	
	def haTerminado(self):
		return self.terminado
	
	@staticmethod
	def comparador(pro1,pro2):
		'''Devuelve positivo, cero o negativo, si el proceso1 mas alta, igual,
		o menor prioridad que proceso2'''
		return pro2.prioridad-pro1.prioridad
	
	def __str__(self):
		return self.obtNombre()
