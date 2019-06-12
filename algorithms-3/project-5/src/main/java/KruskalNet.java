
public class KruskalNet {

	private int sats;
	private int camps;

	private KruskalEdge[] edges;
	private DisjointSetInt set;

	private double minDistance;

	/**
	 * Maneja una red de campamentos usando radios, satelites y la posicion X y Y de
	 * los campamentos. Su utilidad reside en el calculo de la menor distancia
	 * posible para los radios para habilitar comunicaciones entre todos los
	 * campamentos.
	 * 
	 * Construye los datos necesarios para el calculo de la distancia
	 * 
	 * @param sats  los satelites disponibles
	 * @param camps el numero de campamentos
	 * @param pos   (las coordenadas de los campamentos)
	 */
	public KruskalNet(int sats, int[][] pos) {
		this.sats = sats;
		this.camps = pos.length;

		// Build edges
		int edgesSize = (pos.length * (pos.length - 1)) / 2;
		this.edges = new KruskalEdge[edgesSize];
		int k = edgesSize - 1;
		for (int i = pos.length - 2; i >= 0; --i) {
			for (int j = pos.length - 1; j > i; j--) {
				edges[k] = new KruskalEdge(i, j, pos[i][0], pos[i][1], pos[j][0], pos[j][1]);
				--k;
			}
		}

		// Build disjoint set
		set = new DisjointSetInt(camps);

		// Default distance
		minDistance = -1;
	}

	/**
	 * Ejecuta el algoritmo de Kruskal en base a los parametros del problema
	 */
	public void calcDistance() {
		// Ordenar los arcos por orden ascendente
		sortEdges();

		int trees = camps;
		int target = sats + 1;

		for (int i = 0; i < edges.length; ++i) {
			KruskalEdge edge = edges[i];
			int orig = edge.getOrigin();
			int dest = edge.getDest();

			// Si los conjuntos son diferentes
			if (set.find(orig) != set.find(dest)) {
				// Unir los conjuntos
				set.union(orig, dest);
				// Verificar los contadores de arboles
				--trees;
				if (trees == target) { // Se llego a la respuesta
					minDistance = edges[i].getCost();
					break;
				}
			}
		}
	}

	/**
	 * Ordena de forma ascendente los arcos generados en base a su distancia
	 * utilizando una implementacion recursiva de mergeSort
	 */
	private void sortEdges() {
		mergeSort(edges, new KruskalEdge[edges.length], 0, edges.length);
	}

	/**
	 * Metodo Getter
	 * 
	 * @return retorna el valor calculado por kruskal
	 */
	public double getMinDistance() {
		return minDistance;
	}

	/**
	 * Metodo recursivo para la implementacion del mergesort
	 * 
	 * @param A arreglo a ordenar
	 * @param B arreglo auxiliar
	 * @param a posicion inicial de ordenamiento
	 * @param b posicion final (por fuera) del ordenamiento
	 */
	private static void mergeSort(KruskalEdge[] A, KruskalEdge[] B, int a, int b) {
		// Casos Base
		if (a == b || b - a == 1)
			return;

		// int mid = ((a + b) << 1) + ((a + b) & 1);
		int mid = (a + b) / 2;
		mergeSort(A, B, a, mid);
		mergeSort(A, B, mid, b);

		// Mezclar
		int j = a;
		int k = mid;
		for (int i = a; i < b; ++i) {
			// Caso izquierdo
			if (j < mid && k >= b) {
				B[i] = A[j];
				++j;
			}
			// Caso derecho
			if (j >= mid && k < b) {
				B[i] = A[k];
				++k;
			}
			// Caso Ambos
			if (j < mid && k < b) {
				if (A[j].compareTo(A[k]) <= 0) {
					B[i] = A[j];
					++j;
				} else {
					B[i] = A[k];
					++k;
				}
			}
			// Caso Listo
			if (j >= mid && k >= b) {
				break;
			}
		}

		// Devolver al arreglo inicial
		for (int i = a; i < b; ++i) {
			A[i] = B[i];
		}
	}
}
