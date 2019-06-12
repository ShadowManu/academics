/** Archivo: FileManager.java
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

public class FileManager{
	// Atributos
	private BufferedReader bReader;
	private BufferedWriter bWriter;
	/*
	 * Constructor de un manejador.
	 */
	public FileManager(){
		bReader = null;
		bWriter = null;
	}
	
	/**
	 * Inicia la lectura de un archivo.
	 */
	public Object[][] readFile(String fileName){
		
		int numCases, numSat, numCamp;
		int[][] coords;
		String[] lineS;
		Object out[][] = null;		
		try {
			this.bReader = new BufferedReader(new FileReader(fileName));
			
			// Obtengo el numero de casos.
			numCases = Integer.parseInt(bReader.readLine());
			out = new Object[numCases][2];
			// Creo cada caso.
			for (int i=0; i<numCases ; i++){
				lineS = bReader.readLine().split(" ");
				numSat = Integer.parseInt(lineS[0]);
				numCamp = Integer.parseInt(lineS[1]);
				coords = new int[numCamp][2];
				// Copio coordenadas en un arreglo.
				for (int j=0; j<numCamp ; j++){
				
					lineS = bReader.readLine().split(" ");
					coords[j][0] =  Integer.parseInt(lineS[1]);
					coords[j][1] =  Integer.parseInt(lineS[2]);
				}
				out[i][0] = numSat;
				out[i][1] = coords;
			}			
		} catch (IOException e) {
			e.printStackTrace();
		}
		return out;
	}
	
	/**
	 * Escribe una lista de string en un archivo dado.
	 */
	public void writeFile(String fileName, List<String> toWrite) {
		ListIterator<String> wIter = toWrite.iterator();
		try {
			bWriter = new BufferedWriter(new FileWriter(fileName));
			while (wIter.hasNext()) {

				bWriter.write((String) wIter.next());
				bWriter.newLine();
			}
			bWriter.flush();
			bWriter.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}