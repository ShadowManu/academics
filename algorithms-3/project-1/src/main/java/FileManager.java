
/* Archivo: FileManager.java
 * Descripcion: Clase que almacena la informacion de un vertice
 * Autor Inicial: Eduardo Blanco
 * 
 * Autores de Implementacion:
 *	 Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 *	 Cristian Medina | 10-10445 | cmedina270793@gmail.com
 * 
 * Fecha: Nov 2013
 */
import java.io.*;

public class FileManager {
	// Atributos
	private BufferedReader bReader;
	private BufferedWriter bWriter;

	/*
	 * Constructor de un manejador.
	 */
	public FileManager() {
		bReader = null;
		bWriter = null;
	}

	/**
	 * Inicia la lectura de un archivo.
	 */
	public MyList<DigraphHL> readFile(String fileName) {

		MyList<DigraphHL> out = new MyList<DigraphHL>();
		String line;

		try {
			this.bReader = new BufferedReader(new FileReader(fileName));
			while ((line = bReader.readLine()) != null) {
				if (!(line.matches("0"))) {
					DigraphHL d = new DigraphHL();
					int top = Integer.parseInt(line);
					for (int i = 1; i < top + 1; i++) {
						line = bReader.readLine();
						d.add(new Node(Integer.toString(i)));
						if (!(line.matches("0"))) {
							for (String e : line.split(" ")) {
								d.add(new Edge(Integer.toString(i), e));
							}
						}
					}
					out.add(d);
				}
			}

			bReader.close();

		} catch (IOException e) {
			e.printStackTrace();
		}

		return (out);
	}

	public void writeFile(String fileName, List<String> toWrite) {
		ListIterator<String> wIter = toWrite.iterator();
		try {
			bWriter = new BufferedWriter(new FileWriter(fileName));
			while (wIter.hasNext()) {
				bWriter.newLine();
				bWriter.write((String) wIter.next());
			}
			bWriter.flush();
			bWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}