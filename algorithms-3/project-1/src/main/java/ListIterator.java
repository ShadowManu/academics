/**
 * Archivo: List.java
 * Descripcion:  Interfaz que describe un iterador sobre listas.
 * Es una interfaz parametrizada con tipo (clase) E.
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

public interface ListIterator<E> {

	/**
	 * Comprueba que exista un proximo elemento.
	 */
	public boolean hasNext();

	/**
	 * Devuelve el elemento asociado y avanza el iterador.
	 */
	public E next();

	/**
	 * Remueve de la lista el ultimo elemento retornado por next()
	 */
	public void unlink();
}

// End ListIterator.
