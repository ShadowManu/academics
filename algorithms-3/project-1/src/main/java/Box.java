/**
 * Archivo: Box.java
 * Descripcion: Clase que almacena la informacion de una caja en una lista.
 * Esta es una clase parametrizada con tipo (clase) E; i.e., la
 * caja contiene elementos de tipo E.
 *
 * Autores de Implementacion:
 *     Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *     Cristian Medina | 10-10445 | cmedina270793@gmail.com
 *
 * Fecha: Nov 2013
 */

class Box<E>{
	
	private E elem;
	private Box<E> next;
	
	/**
	 * Constructor de argumento unico de la clase Box
	 */
	public Box(E element){
		this(element,null);
	}
	
	/**
	 * Constructor completo de la clase Box
	 */
	public Box(E element, Box<E> next) {
		this.elem = element;
		this.next = next;
	}
	
	/**
	 * Obtiene el elemento de la caja
	 */
	public E getElem(){
		return this.elem;
	}
	
	/**
	 * Obtiene el enlace 
	 */
	public Box<E> getNext(){
		return this.next;
	}
	
	/**
	 * Modifica el enlace 
	 */
	public void setNext(Box<E> next){
		this.next = next;
	}
}

// End Box.java

