/**
 * Archivo: HashTree.java
 * Descripcion: Clase de implementacion de un Hash de Nodos
 * que maneja colisiones mediante listas (usando MyList<Node>)
 * de elementos genericos
 * 
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 * 
 * Fecha: Nov 2013
 */

public class HashList {
	
	Object[] data;
	int tam = 10;
	int elems = 0;
	private static final int maxCols = 5;
	
	public HashList() {
		data = new Object[tam];
	}
	
	public boolean contains() {
		// #! No Listo
		return true;
	}
	
	@SuppressWarnings("unchecked")
	public boolean add(Node element) {
		int pos = hashFunction(element);
		System.out.println(pos);
		MyList<Node> list;
		
		// Caso nuevo elemento en la posicion
		if (data[pos] == null) {
			list = new MyList<Node>();
			list.add(element);
			++elems;
			
			data[pos] = list;
			return true;
		
		// Caso Colision
		} else {
			list = (MyList<Node>) data[pos];
			if (list.contains(element)) return false;
			list.add(element);
			++elems;
			
			// Una lista comienza a hacerse muy larga
			if (list.getSize() > maxCols) {
				reHash();
			}
			return true;
		}
	}
	
	private int hashFunction(Node element) {
		int temp = element.toString().hashCode() % tam;
		if (temp < 0) {
			return -temp;
		}
		return temp;
	}
	
	public Node get(Node node) {
		HashListIterator hashIter = iterator();
		MyList<Node> inList;
		ListIterator<Node> ListIter;
		
		// Por cada lista de nodos
		while (hashIter.hasNext()) {
			inList = hashIter.next();
			ListIter = inList.iterator();
		
			// En cada uno de sus nodos
			while (ListIter.hasNext()) {
				Node element = ListIter.next();
				
				// Si encuentro un nodo con el mismo id
				if (element.toString().equals(node.toString())) {
					return element;
				}
			}
		}
		return null;
	}
	
	public boolean remove(Node node) {
		HashListIterator hashIter = iterator();
		MyList<Node> inList;
		ListIterator<Node> ListIter;
		
		// Por cada lista de nodos
		while (hashIter.hasNext()) {
			inList = hashIter.next();
			ListIter = inList.iterator();
		
			// En cada uno de sus nodos
			while (ListIter.hasNext()) {
				Node element = ListIter.next();
				
				// Si encuentro un nodo con el mismo id
				if (element.toString().equals(node.toString())) {
					ListIter.unlink();
					--elems;
					return true;
				}
			}
		}
		return false;
	}
	
	@SuppressWarnings("unchecked")
	private void reHash() {
		HashListIterator oldIter = iterator();
		MyList<Node> inList;
		ListIterator<Node> inIter;
		
		// Calcular nuevo tamano
		if (elems < 500) {
			tam = elems << 1;
		} else {
			tam = tam + 500;
		}
		// El metodo hashFunction() ahora se comporta diferente
		
		Object[] newData = new Object[tam];
		
		// Por cada lista de nodos
		while (oldIter.hasNext()) {
			inList = oldIter.next();
			inIter = inList.iterator();
			
			// En cada uno de sus nodos
			while (inIter.hasNext()) {
				Node element = inIter.next();
				
				int pos = hashFunction(element);
				MyList<Node> list;
				
				// Caso nuevo elemento en la posicion
				if (newData[pos] == null) {
					list = new MyList<Node>();
					list.add(element);
					
					newData[pos] = list;
				
				// Caso Colision
				} else {
					list = (MyList<Node>) newData[pos];
					list.add(element);
				}
			}	
		}
		
		// newData esta completo
		data = newData;
	}
		
	public HashListIterator iterator() {
		class HLIterator implements HashListIterator {
			Object[] hash = data;
			int pos = 0;
			
			public boolean hasNext() {
				for (int i = pos; i < tam; ++i) {
					if (hash[i] != null) {
						pos = i;
						return true;
					}
				}
				return false;
			}
			
			@SuppressWarnings("unchecked")
			public MyList<Node> next() {
				for (int i = pos; i < tam; ++i) {
					if (hash[i] != null) {
						pos = i+1;
						return (MyList<Node>) hash[i];
					}
				}
				throw new IllegalStateException();
			}
		}
		
		// End of HLIterator
		
		return new HLIterator();
	}
	

}