/**
 * Archivo: EdgeTester.java
 * Descripcion: Clase de prueba para la clase Edge
 * 
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 * 
 * Fecha: Nov 2013
 */

public class EdgeTester {
	
	public static void main(String[] args) {
		
		System.out.println("\nINICIO DE LA PRUEBA DE LA CLASE EDGE");
		
		System.out.println("\nPRUEBA DE Edge()\n");
		
		System.out.println("Nuevo Edge A con 'Ini' y 'Fin'");
		Edge edgeA = new Edge("Ini","Fin");
		
		System.out.println("Nuevo Edge B con 'Cosa' y 'Coso'");
		Edge edgeB = new Edge("Cosa","Coso");
		
		System.out.println("Nuevo Edge C con 'Ini' y 'Fin'");
		Edge edgeC = new Edge("Ini","Fin");
		
		System.out.println("\nPRUEBA DE toString()\n");
		
		System.out.println("Impresion de Edge A, B y C:\n" +
			edgeA.toString() + " " +
			edgeB.toString() + " " +
			edgeC.toString() + ".");
		
		System.out.println("\nPRUEBA DE equals()\n");
		
		System.out.println( edgeA.equals(edgeB) ?
			"Los Edge A y B son iguales." :
			"Los Edge A y B son diferentes.");
		
		System.out.println( edgeA.equals(edgeC) ?
			"Los Edge A y C son iguales." :
			"Los Edge A y C son diferentes.");
		
		System.out.println( edgeA.equals(edgeA) ?
			"El Edge A es igual a si mismo." :
			"El Edge A no es igual a si mismo.");
		
		System.out.println("\nPRUEBA DE clone(), getSrc(), getDst().\n");
		
		System.out.println("Edge D es una copia de Edge B.");
		Edge edgeD = (Edge) edgeB.clone();
		
		System.out.println("Fuente de Edge D: " + edgeD.getSrc());
		System.out.println("Destino de Edge D: " + edgeD.getDst());
		
		System.out.println("FIN DE LA PRUEBA DE LA CLASE EDGE");
	}
}

// End EdgeTester