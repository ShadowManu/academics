/**
 * Archivo: KruskalEdge.java
 * Descripcion: Clase que define la estructura arco utilizada para la
 * ejecucucion del algoritmo de Kruskal.
 * 
 * Autores de Implementacion:
 *     Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *     Cristian Medina | 10-10445 | cmedina270793@gmail.com
 *
 * Fecha: Enero 2014
 */
public class KruskalEdge implements Comparable<KruskalEdge> {
	
	private int origin;
	private int dest;
	private double cost;
	
	/**
	 * Construye el arco incluyendo el calculo de su costo
	 * @param origin casilla (id) asignada al origen
	 * @param dest casilla (id) asignada al destino
	 * @param x1 posicion del origen en el eje X
	 * @param y1 posicion del origen en el eje Y
	 * @param x2 posicion del destino en el eje X
	 * @param y2 posicion del destino en el eje Y
	 */
	public KruskalEdge(int origin, int dest, int x1, int y1, int x2, int y2) {
		this.origin = origin;
		this.dest = dest;
		
		int xDiff = x2 - x1;
		int yDiff = y2 - y1;
		int base = xDiff * xDiff + yDiff * yDiff;
		this.cost = Math.round(Math.sqrt(base));
	}
	
	/**
	 * Metodo getter
	 * @return la casilla (id) asignada al origen del arco
	 */
	public int getOrigin() {
		return this.origin;
	}

	/**
	 * Metodo getter
	 * @return la casilla (id) asignada al destino del arco
	 */
	public int getDest() {
		return this.dest;
	}
	
	/**
	 * Metodo getter
	 * @return el costo relacionado al arco
	 */
	public double getCost() {
		return this.cost;
	}

	/**
	 * Nota: La comparacion esta basada en el costo del arco 
	 */
	@Override
	public int compareTo(KruskalEdge edge) {
		if (this.cost > edge.cost) {
			return 1;
		} else if (this.cost == edge.cost) {
			return 0;
		} else if (this.cost < edge.cost) {
			return -1;
		}
		return 0;
	}
}
