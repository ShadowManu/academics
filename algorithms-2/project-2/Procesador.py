'''
Created on Jun 4, 2014

@author: Manuel Pacheco
'''

from Proceso import Proceso

class Procesador:
	'''
	classdocs
	'''
	STR_SUBP_EMPILAR = ('%d Empilando %d subprocesos del proceso %s'
						' en el procesador %d.')

	def __init__(self,iden,simulador=None):
		'''
		Constructor
		'''
		# Atributos principales
		self.id = int(iden)
		self.procesosEmpilados = []
		
		# Atributos auxiliares
		self.ocupado = False
		self.memoriaAsignada = 0
		self.simulador = simulador
				
	def obtId(self):
		return self.id

	def obtMemoriaAsignada(self):
		return self.memoriaAsignada
	
	def obtSimuladorAsociado(self):
		return self.simulador
	
	def obtProcesosEmpilados(self):
		return self.procesosEmpilados
	
	def estaOcupado(self):
		return self.ocupado
	
	def resignarMemoria(self):
		self.memoriaAsignada = 0;

	def asignarProceso(self,proceso):
		self.ocupado = True
		self.memoriaAsignada = proceso.obtMemoria()
		self.procesosEmpilados.append(proceso)
		proceso.enlazarProcesador(self)
		
		# Agregar subprocesos (si los hay)
		if proceso.obtSubprocesos() != 0:
			
			# Impresion de empilacion
			print(self.STR_SUBP_EMPILAR % (
					self.simulador.obtTiempo(),	# Tiempo
					proceso.obtSubprocesos(),	# No. de subprocesos
					proceso.obtNombre(),		# Nombre del proceso
					self.id))					# Numero de procesador
			
			for i in range(proceso.obtSubprocesos()):
				self.procesosEmpilados.append(
						# Hereda los atributos del proceso padre
						Proceso(proceso.obtNombre()+'-'+str(i),
								proceso.obtPrioridad(),	# Prioridad
								0,						# No usa memoria
								proceso.obtDuracion(),	# Duracion
								0,						# No tiene subprocesos
								self))					# Procesador asociado

	def simularTiempo(self):
		self.procesosEmpilados[0].simularTiempo()
		# Si el proceso actual ha terminado
		if self.procesosEmpilados[0].haTerminado():
			# Remover de la pila
			del(self.procesosEmpilados[0])
			# Revisar si aun hay procesos
			if not self.procesosEmpilados:
				self.ocupado = False
		
	def __str__(self):
		return str(self.id)
		
