/**
 * Archivo: Node.java
 * Descripcion: Clase que almacena la informacion de un vertice
 * Autor Inicial: Eduardo Blanco
 * 
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 * 
 * Fecha: Nov 2013
 */

public class Node{

	// Se asume que el id es unico
	private String id = null;
	private MyList<Edge> inEdge = new MyList<Edge>();
	private MyList<Edge> outEdge = new MyList<Edge>();
	
	public Node(String i){
		id = new String(i);
	}

	/**
	 * Retorna una nuevo nodo que es copia de this.
	 */
	@Override
	protected Object clone() {
		return new Node(id);
	}

	/**
	 * Indica si la arista de entrada es igual a this.
	 */
	public boolean equals(Object o) {
		Node d;

		if (!(o instanceof Node)) {
			return false;
		}
		d = (Node) o;

		return d.id.equalsIgnoreCase(id);
	}

	/**
	 * Retorna la representacion en String de la arista.
	 */
	@Override
	public String toString() {
		return new String(id);
	}

	/**
	 * Retorna la lista de arcos internos
	 */
	public MyList<Edge> getIE(){
		return this.inEdge;
	}
	
	/**
	 * Retorna la lista de arcos externos
	 */
	public MyList<Edge> getOE(){
		return this.outEdge;
	}
	
	/**
	 * Agrega un arco a los arcos internos
	 */
	public boolean addInEdge(Edge e){
		return this.inEdge.add(e);
	}
	
	/**
	 * Agrega un arco a los arcos externos
	 */
	public boolean addOutEdge(Edge e){
		return this.outEdge.add(e);
	}
}

// End Node.java
