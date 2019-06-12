/**
 * Archivo: Edge.java
 * Descripcion: Clase que implementa la interfaz List
 * Esta es una clase parametrizada con tipo (clase) E; i.e., la
 * lista contiene elementos de tipo E (en la implementacion Box<E>).
 *
 * Autor Inicial: Eduardo Blanco
 *
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 *
 * Fecha: Nov 2013
 */

public class MyList<E> implements List<E>{

	/*
	 * Modelo de representacion: lista enlazada.
	 * Se usa la clase Box como wrapper de E
	 */

	private int size;
	private Box<E> head;
	private Box<E> tail;

	/**
	 * Constructor de la clase MyList
	 */
	public MyList() {
		size = 0;
	}

	/**
	 * Agrega un elemento al final de la lista.
	 */
	public boolean add(E element) {
		// Hacer el wrapper
		Box<E> box = new Box<E>(element,null);
		
		// Si la lista es vacia
		if (tail == null) {
			tail = box;
			head = box;
			++size;
		
		// Si ya contiene elementos
		} else {
			tail.setNext(box);
			tail = box;
			++size;
		}
		return true;
		
	}

	/**
	 * Elimina todos los elementos de la lista. La lista queda
	 * como recien creada.
	 */
	public void clear() {
		// GC de Java deberia encargarse de la eliminacion
		// dado que el Box<E> de head no tiene referencias vivas
		head = null;
		tail = null;
		size = 0;
	}

	/**
	 * Determina si el elemento dado esta en la lista.
	 */
	public boolean contains(Object element){
		ListIterator<E> iter = iterator();

		while (iter.hasNext()) {
			if (iter.next().equals(element)) return true;
		}
		return false;
	}

	/**
	 * Determina si la lista dada es igual a la lista.
	 */
	@SuppressWarnings("rawtypes")
	public boolean equals(Object o) {
		if (this == o) return true;					// Misma entidad
		if (!(o instanceof List)) return false;		// Objetos incompatibles
		
		List list = (List) o;
		if (this.getSize() != list.getSize());		// Tamanos diferentes
		
		ListIterator i1 = this.iterator();
		ListIterator i2 = list.iterator();
		while (i1.hasNext() && i2.hasNext()) {
			
			// "Objeto" diferente
			if (!( i1.next().equals(i2.next()) )) {
				return false;
			}
		}
		
		return true;
	}

	/**
	 * Determina si la lista es vacia.
	 */
	public boolean isEmpty() {
		return size == 0;
	}

	/**
	 * Retorna el elemento en la posicion pos,
	 * 0 <= pos < this.getSize()
	 */
	public E get(int pos) {
		ListIterator<E> iter = iterator();
		E out = null;
		
		if (pos >= size) {
			throw new IllegalStateException();
		}
		
		while (pos >= 0) {
			out = iter.next();
			--pos;
		}
		return out;
	}

	/**
	 * Retorna el elemento elem, si existe.
	 * De lo contrario, null.
	 */
	public E get(E element) {
		ListIterator<E> iter = iterator();
		E out;

		while (iter.hasNext()) {
			out = iter.next();
			if (out.equals(element)) {
				return out;
			}
		}
		return null;
	}
	
	/**
	 * Elimina el elemento que esta en la posicion pos de la lista. Si
	 * la lista cambia, retorna true, sino retorna false.  
	 *
	 * Utilizar esta operacion puede hacer invalido los iteradores
	 * sobre this
	 */
	public boolean remove(int pos){
		// Descartar valor invalido
		if (pos >= size) {
			return false;
		}
		
		ListIterator<E> iter = iterator();
		
		// Ejecutar next(), pos veces
		while(pos >= 0) {
			iter.next();
			--pos;
		}
		
		// Remover elemento pos
		iter.unlink();
		
		return true;
	}

	/**
	 * Elimina el elemento dado de la lista. Si la lista cambia,
	 * retorna true, sino retorna false.
	 */
	public boolean remove(E element){
		ListIterator<E> iter = iterator();
		
		while (iter.hasNext()) {
			if(iter.next().equals(element)) {
				iter.unlink();
				return true;
			}
		}
		return false;
	
		
	}

	/**
	 * Retorna el numero de elementos en la lista
	 */
	public int getSize(){
		return size;
	}

	/**
	 * Devuelve un iterador sobre la lista.
	 */
	public ListIterator<E> iterator() {
		class InnerIterator implements ListIterator<E> {
			private Box<E> next;
			private Box<E> last;
			private Box<E> last2;
			
			/**
			 * Constructor
			 */
			public InnerIterator() {
				next = head;
			}
			
			/**
			 * Comprueba que exista un proximo elemento.
			 */
			public boolean hasNext() {
				return next != null;
			}
			
			/**
			 * Devuelve el elemento asociado y avanza el iterador.
			 */
			public E next() {
				if (last != null) last2 = last;
				last = next;
				next = next.getNext();
				return last.getElem();
			}

			/**
			 * Remueve de la lista el ultimo elemento retornado por next()
			 */
			public void unlink() {
				// No se ha hecho next() o ya se ha hecho unlink()
				if (last == null) {
					throw new IllegalStateException();
				}
				
				last = null;
				
				// Si es el primer elemento
				if (last2 == null) {
					head = next; 
				
				// O es en cualquier otra posicion
				} else {
					last2.setNext(next);
				}
				
				// Si por otra parte es el ultimo elemento
				if (next == null) {
					tail = last2;		// No hay problema que last2 sea null
										// ya que entonces la lista queda vacia
				}
				
				// Ajustar el nuevo tamano
				--size;
			}
			
		}
		// End InnerIterator
		
		return new InnerIterator();
		
	}
}

// End List.
