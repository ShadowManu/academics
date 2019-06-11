/**
 * Archivo: MyList.java
 * Descripcion: Clase de prueba para la clase MyList
 * 
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 * 
 * Fecha: Nov 2013
 */

public class MyListTester {

	public static void main(String[] args) {
		
		System.out.println("\nINICIO DE LA PRUEBA DE LA CLASE MYLIST");
		System.out.println("Se probara sobre una lista de strings\n");
		
		System.out.println("\nPRUEBA DE MyList() y isEmpty()\n");
		MyList<String> lista = new MyList<String>();
		System.out.println("Lista Creada");
		System.out.println(lista.isEmpty() ? "Es vacia" : "No es vacia");
		
		System.out.println("\nPRUEBA DE add() y isEmpty()\n");
		
		System.out.println("Se agregan los elementos 'hola', 'cosa', 'cosito', 'perro', 'cosa'");
		// Se ob
		lista.add("hola");
		lista.add("cosa");
		lista.add("cosito");
		lista.add("perro");
		lista.add("casa");
		System.out.println(lista.isEmpty() ? "Ahora es vacia" : "Ahora no es vacia");
		System.out.println("\nEstado:");
		printStrList(lista);

		
		System.out.println("\nPRUEBA DE contains()\n");
		System.out.println(lista.contains("cosit" + "o") ?
			"La lista contiene al String 'cosito'" :
			"La lista no contiene al String 'cosito'");
		System.out.println(lista.contains("gato") ?
				"La lista contiene al String 'gato'" :
				"La lista no contiene al String 'gato'");
		
		System.out.println("\nPRUEBA DE get() y getSize()\n");
		System.out.println("El tamano de la lista es " + lista.getSize());
		System.out.println("EL penultimo elemento de la lista es " + lista.get(lista.getSize() -2));
		
		System.out.println("\nPRUEBA DE remove(int)\n");
		System.out.println("Se elimina el tercer elemento");
		lista.remove(2);
		System.out.println("\nEstado:");
		printStrList(lista);
		
		System.out.println("\nPRUEBA DE remove(E)\n");
		System.out.println("Se elimina perro");
		lista.remove("perro");
		System.out.println("\nEstado:");
		printStrList(lista);
		
		System.out.println("\nPRUEBA DE clear()\n");
		lista.clear();
		System.out.println("Estado:");
		printStrList(lista);
		System.out.println(lista.isEmpty() ? "Ahora es vacia" : "Ahora no es vacia");
				
		System.out.println("\nPRUEBA DE equals\n");
		
		MyList<String> lista2 = new MyList<String>();
		lista.add("dataA");
		lista.add("dataB");
		lista2.add("data" + "A");
		lista2.add("data" + "C");
		lista2.add("data" + "B");
		lista2.remove(1);
		
		System.out.println(lista.equals(lista2) ?
			"Las 2 listas creadas son iguales" :
			"Las 2 listas creadas no son iguales");
		
		System.out.println("\nFIN DE LA PRUEBA DE LA CLASE EDGE");
	}
	
	/**
	 * Impresion de los elementos de una lista MyList usando MyList.iterator()
	 */
	static void printStrList(MyList<String> list) {
		
		// PRUEBA IMPLICITA DE iterator() (aunque tambien se prueba en remove() )
		ListIterator<String> iter = list.iterator();
		
		while (iter.hasNext()) {
			System.out.print(iter.next() + ", ");
		}
		System.out.println();
	}
}

// End MyListTester
