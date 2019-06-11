/**
 * Archivo: Edge.java
 * Descripcion: Clase que almacena la informacion de una Arista (lado)
 * Autor Inicial: Eduardo Blanco
 * 
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 * 
 * Fecha: Nov 2013
 */

public class Edge {

	private String src = null;
	private String dst = null;

	/**
	   Constructor de uso exclusivo de la clase edge.
	 */
	private Edge() {
	}
	/**
	 * Crea una arista entre los vertices src y dst.
	 */
	public Edge(String src, String dst) {
		this.src = new String(src);
		this.dst = new String(dst);
	}

	/**
	 * Retorna una nueva arista que es copia de this.
	 */
	@Override
	protected Object clone() {
		Edge ed = new Edge();

		// Se copian (clonan) todos los objetos internos, 
		// no solo asignar las referencias
		ed.src = new String(src);
		ed.dst = new String(dst);
		
		return ed;
	}

	/**
	 * Indica si la arista de entrada es igual a this.
	 */
	public boolean equals(Object o) {
		if (this == o) return true;						// Misma entidad
		if ( !(this instanceof Edge) ) return false;	// Objetos incompatibles
		Edge obj = (Edge) o;
		return 
			this.src.equals(obj.src) &&
			this.dst.equals(obj.dst);
	}

	/**
	 * Retorna el vertice src de la arista.
	 */
	public String getSrc() {
		return (this.src);
	}

	/**
	 * Retorna el vertice dst de la arista.
	 */
	public String getDst() {
		return (this.dst);
	}
	
	/**
	 * Retorna la representacion en String de la arista.
	 */
	@Override
	public String toString() {
	return "(" + src + ", " + dst + ")";
	}

}

// End Edge.java
