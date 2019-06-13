'''
Created on Jun 4, 2014

@author: Manuel Pacheco
'''

from Proceso import Proceso
from Procesador import Procesador
from ColaPrioridades import ColaPrioridades

class Simulador:
	'''Clase principal del proyecto donde se implementa el simulador'''

	numProcesadores = 4
	memoria = 1024

	def __init__(self,archivoEntrada):
		'''Constructor'''
		# Crear los procesadores del simulador
		self.procesadores = []
		for i in range(Simulador.numProcesadores):
			self.procesadores.append(Procesador(i+1,self))
		
		self.colaProcesos = ColaPrioridades(Proceso.comparador)
		self.tiempo = 1
		self.memoriaEnUso = 0
		
		self.leerEntrada(archivoEntrada)
	
	def obtTiempo(self):
		return self.tiempo
	
	def procesadorLibre(self):
		'''Retorna el numero del primer procesador que esta libre
		o -1 si no hay ninguno'''
		for (indice, procesador) in enumerate(self.procesadores):
			if not procesador.estaOcupado():
				return indice
		return -1
	
	def asignarProceso(self,proceso,procesador):
		self.procesadores[procesador].asignarProceso(proceso)
		
	def simular(self):
		while (True):	# Es indefinido cuanto tiempo sera la simulacion
			
			# Impresion de listado
			if (self.tiempo % 100) == 0:
				self.imprimirListado()

			# Si todos los procesos han sido procesados, salir
			listo = True
			if len(self.colaProcesos) != 0:		# Procesos en cola
				listo = False
			for procesador in self.procesadores:
				if procesador.estaOcupado():	# Procesos en procesador
					listo = False
			if listo:
				# Imprimir fin
				print('%d Fin' % self.tiempo)
				break
			
			# Asignar procesos si hay procesadores libres
			i = self.procesadorLibre()
			while i >= 0:				
				# Buscar un proceso para asignar
				encontrado = False
				for j in range(len(self.colaProcesos)):
					proc = self.colaProcesos.obtener(j)
					# Revisar que la memoria esta disponible
					if proc.obtMemoria() + self.memoriaEnUso <= self.memoria:
						encontrado = True
						self.asignarProceso(proc, i)
						self.colaProcesos.remover(j)
						# Asignar memoria
						self.memoriaEnUso += proc.obtMemoria()
						break
				# No hay procesos que caben en la memoria
				if not encontrado:
					break
				# Siguiente procesador libre
				i = self.procesadorLibre()
			
			# Simular tiempo en los procesadores
			for procesador in self.procesadores:
				if procesador.estaOcupado():
					procesador.simularTiempo()
			
			# Liberar memoria
			for procesador in self.procesadores:
				if not procesador.estaOcupado():
					self.memoriaEnUso -= procesador.obtMemoriaAsignada()
					procesador.resignarMemoria()
			
			# Incrementar tiempo del simulador
			self.tiempo += 1

	def imprimirListado(self):
		# Obtener los procesos en cola y en procesadores
		procesos = []
		procesos.extend(self.colaProcesos.obtElementos())
		for procesador in self.procesadores:
			procesos.extend(procesador.obtProcesosEmpilados())
		
		# Ordenar los procesos en base a su nombre
		procesos.sort(key=lambda x: x.obtNombre().lower(), reverse=False)
		
		# Impresion de inicio
		print ('%d Listado' % self.tiempo)
		
		# Imprimir la informacion de cada proceso
		for proceso in procesos:
			# Definir el estado
			if proceso.obtProcesadorAsociado() == None:
				estado = 'Cola de prioridades'
			else:
				estado = ('Procesador %d' %
						proceso.obtProcesadorAsociado().obtId())
			
			# Imprimir la linea
			print('%s %d %d %d %s' % (
					proceso.obtNombre(),
					proceso.obtPrioridad(),
					proceso.obtMemoria(),
					proceso.obtDuracion(),
					estado))
		
		# Impresion de fin
		print('Fin listado')

	def leerEntrada(self,nombreEntrada):
		try:
			for line in open(nombreEntrada):
				args = line.split()
				
				if args[0] != 'proceso':
					raise ValueError('Error de sintaxis en archivo de entrada')
				if len(args) != 6:
					raise ValueError('Numero incorrecto de parametros')
				
				self.colaProcesos.agregar(Proceso(args[1],	# Nombre
												  args[2],	# Prioridad
												  args[3],	# Memoria
												  args[4],	# Duracion
												  args[5],	# Subprocesos
												  None))	# Procesador
		except IOError:
			raise IOError('No se encuentra el archivo %s' % nombreEntrada)