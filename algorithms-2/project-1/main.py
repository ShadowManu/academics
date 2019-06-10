class Tabla:
  """
  Implementacion de un TDA que maneja un conjuntos de entradas
  de tipo (key,value). Basada en la implementacion de diccionario
  nativa de python
    
  Autores:
    Manuel Pacheco
    Cristian Medina
  """
    
  def __init__(self,dic=None):
    """
    Inicializa una tabla a partir de un diccionario
    (vacio por default).
    """
    self.t = dic if dic != None else dict()
    
  
  def mostrar(self):
    """Imprime el diccionario usando el built-in print()."""
    print(self.t)
  
  def leerdict(self,nombre):
    """Agrega entradas al diccionario a partir de un nombre de archivo."""
    # Obtener lineas		
    with open(nombre) as f:
      lineas = f.readlines()
    
    # Procesar linea		
    for linea in lineas:
      linea = linea.strip('\n')
      if linea == "":
        break
      
      items = linea.split()
      self.t[items[0]] = items[1]

  def Puntos_Fijos(self):
    """Retorna una Tabla con los Puntos Fijos de self."""
    out = dict();

    # Agregar elementos a out que tienen igual clave en t
    for elem in self.t.keys():
      if elem == self.t[elem]:
        out[elem] = self.t[elem];

    return Tabla(out);

  def Puntos_Movibles(self):
    """Retorna una Tabla con los Puntos Movibles de self."""
    out = dict()

    # Agregar elementos a out que tienen diferente clave en t
    for elem in self.t.keys():
      if elem != self.t[elem]:
        out[elem] = self.t[elem];

    return Tabla(out);
      
  def Puntos_Potencia(self,n):
    """Retorna una Tabla con los Puntos Potencia de self."""
    out = dict();

    # Para cada elemento de out
    for key in self.t.keys():
      
      # Iterar la busqueda sobre la tabla			
      times = n;
      value = key;
      while times and value != None:
        value = self.t.get(value)
        times = times - 1
      
      # Agregar a la salida si la busqueda n veces fue exitosa
      if value:
        out[key] = value

    return Tabla(out)

  @staticmethod
  def Puntos_k_Estacionarios(ListaTablas):
    """Retorna una Tabla con los Puntos k-Estacionarios de una Lista de Tablas."""
    out = dict()
    
    # Para cada llave en la 1ra tabla
    for key in ListaTablas[0].t.keys():
      value = key

      # Iterar las busquedas en las tablas
      for i in range(len(ListaTablas)):
        value = ListaTablas[i].t.get(value)
        if not value:
          break
      
      # Agregar a la tabla de salida si es valido
      # y ademas son puntos fijos
      if value and key == value:
        out[key] = value

    return Tabla(out)

  @staticmethod
  def Tope_k_Reflexivo(key,ListaTablas):
    """
    Retorna una Tabla con el Tope k-Reflexivo de una Lista de Tablas
    de un elemento key.
    """
    # Obtener el maximo No. de iteraciones 
    max = len(ListaTablas[0].t)
    for tabla in ListaTablas:
      if len(tabla.t) < max:
        max = len(tabla.t)

    value = key;
    periodoR = 0

    # Por un numero maximo de veces
    for i in range(max):
      periodoR = periodoR+1

      # A traves de cada tabla
      for tabla in ListaTablas:

        # Intentar ejecutar la busqueda
        if value in tabla.t.keys():
          value = tabla.t[value]

          # Y revisar la reflexividad
          if key == value:
            break
          
        # Si no fue posible
        else:
          # El periodo ya es inf
          periodoR = "inf"
          break

      # Dejar de hacer los recorridos
      # si ya obtuve un resultado
      if (periodoR == "inf") or (key == value):
        break

    return periodoR

        
### TEST SCRIPT ###

print("***** CLASE TABLA *****")

# Lista de los nombres de archivos de las tablas
NombresTablas = [
  "CasoPrueba.txt",
  # "Tabla_B.txt",
  # "Tabla_C.txt",
  # "Tabla_D.txt"
]

# Creacion de la lista de Tablas
ListaTablas = []
for nombre in NombresTablas:
  temp = Tabla()
  temp.leerdict(nombre)
  ListaTablas.append(temp)

# Pruebas sobre las tablas

for tabla in ListaTablas:
  print("\nImpresion Base")
  tabla.mostrar()

  print("\nImpresion Fijos")
  tabla.Puntos_Fijos().mostrar()

  print("\nImpresion Movibles")
  tabla.Puntos_Movibles().mostrar()

  print("\nImpresion Potencia")
  tabla.Puntos_Potencia(5).mostrar()


print("\nImpresion k_Estacionarios")
Tabla.Puntos_k_Estacionarios(ListaTablas).mostrar()

print("\nImpresion k_Estacionarios")
print(Tabla.Tope_k_Reflexivo("Cambur",ListaTablas))

print("\nFin del Programa\n")