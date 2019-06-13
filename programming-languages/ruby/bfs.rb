module Bfs
  def find(predicate)
    # Iniciar la cola y los elementos marcados
    queue = [self]
    visited = []

    # Mientras la cola no este vacia
    while !queue.empty?
      # Obtener el primer elemento
      obj = queue.shift
      # Revisar si cumple con el predicado
      return obj if obj.value == predicate
      # Marcarlo si no
      visited << obj
      # Agregar sus hijos no marcados a la cola
      obj.each do |child|
        queue << child unless visited.include?(child)
      end
    end
    # Retornar nil si nunca se encontró
    nil
  end

  def path(predicate)
    # Iniciar la cola y los elementos marcados
    queue = [[self,[]]]
    visited = []

    # Mientras la cola no este vacia
    while !queue.empty?
      # Obtener el primer elemento con su camino
      obj, way = queue.shift
      # Revisar si cumple con el predicado
      return way << obj if obj.value == predicate
      # Marcarlo si no
      visited << obj
      # Agregar sus hijos no marcados a la cola
      way << obj
      obj.each do |child|
        queue << [child, Array.new(way)] unless visited.include?(child)
      end
    end
    # Retornar nil si nunca se encontró
    nil
  end

  def walk(&action)
    # Si no nos dan el bloque
    if !block_given?
      # Retornar el arreglo con todos los elementos
      out = []
      walk { |elem| out << elem }
      return out
    end

    # Iniciar la cola y los elementos marcados
    queue = [self]
    visited = [self]

    # Mientras la cola no este vacia
    while !queue.empty?
      # Obtener el primer elemento
      obj = queue.shift
      # Dar el elemento al bloque de codigo
      yield obj
      # Agregar sus hijos no marcados a la cola
      obj.each do |child|
        queue << child unless visited.include?(child)
        visited << child
      end
    end
  end
end

class BinTree
  attr_accessor :value,   # Valor almacenado en el nodo
                :left,    # BinTree izquierdo
                :right    # BinTree derecho

  def initialize(v, l=nil, r=nil)
    @value = v
    @left = l
    @right = r
  end

  def each(&block)
    yield left unless left.nil?
    yield right unless right.nil?
  end
end

class GraphNode
  include Bfs
  attr_accessor :value, # Valor almacenado en el nodo
                :children # Arreglo de sucesores GraphNode

  def initialize(v, c=[])
    @value = v
    @children = c
  end

  def each(&block)
    @children.each do |c|
      yield c
    end
  end
end