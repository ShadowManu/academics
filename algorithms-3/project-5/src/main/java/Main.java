/**
 * Archivo: Main.java Descripcion: Clase principal para la ejecucion del
 * programa que resuelve los problemas planteados en el proyecto
 *
 * Autores de Implementacion: Manuel Pacheco | 10-10524 |
 * manuelalejandropm@gmail.com Cristian Medina | 10-10445 |
 * cmedina270793@gmail.com
 *
 * Fecha: Dic 2013
 */
public class Main {

	public static void main(String[] args) {
		FileManager fman = new FileManager(); // Manejador de archivos
		Object[][] read;
		KruskalNet k; // Grafo que se le aplica el kruskal

		List<String> lstr = new MyList<String>(); // Lista de Resultados

		// Si no se dan los argumentos correctamente
		/*
		 * if (args.length != 2) {
		 * System.out.println("Sintaxis Erronea. Uso: main <entrada> <salida>");
		 * System.exit(-1); }
		 */

		// Se lee el archivo
		read = fman.readFile("01.in");
		for (int i = 0; i < read.length; i++) {
			k = new KruskalNet((int) read[i][0], (int[][]) read[i][1]);
			k.calcDistance();
			lstr.add(String.valueOf(k.getMinDistance()));
		}

		// Imprimir todos los resultados
		fman.writeFile("chao.txt", lstr);

		// Tambien en consola
		ListIterator<String> lIter = lstr.iterator();
		while (lIter.hasNext()) {
			System.out.println(lIter.next());
		}
	}
}