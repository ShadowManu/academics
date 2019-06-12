/**
 * Archivo: List.java
 * Descripcion: Interfaz que define el comportamiento de una lista
 *
 * Esta es una clase parametrizada con tipo (clase) E; i.e., la
 * lista contiene elementos de tipo E.
 *
 * Autor Inicial: Eduardo Blanco
 *
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 *
 * Fecha: Nov 2013
 */

public interface List<E> {

	/**
	 * Agrega un elemento a la lista.
	 */
	public boolean add(E element);

	/**
	 * Elimina todos los elementos de la lista. La lista queda
	 * como recien creada.
	 */
	public void clear();

	/**
	 * Determina si el elemento dado esta en la lista.
	 */
	public boolean contains(Object element);

	/**
	 * Determina si la lista dada o es igual a la lista this.
	 */
	public boolean equals(Object o);

	/**
	 * Determina si la lista es vacia.
	 */
	public boolean isEmpty();

	/**
	 * Elimina el elemento que esta en la posicion pos de la lista. Si
	 * la lista cambia, retorna true, sino retorna false.
	 */

	public boolean remove(int pos);
	/**
	 * Elimina el elemento dado de la lista. Si la lista cambia,
	 * retorna true, sino retorna false.
	 */
	public boolean remove(E element);

	/**
	 * Retorna el numero de elementos en la lista
	 */
	public int getSize();

	/**
	 * Devuelve un iterador sobre la lista.
	 */
	public ListIterator<E> iterator();

}

// End List.
