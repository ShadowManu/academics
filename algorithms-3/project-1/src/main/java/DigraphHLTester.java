
public class DigraphHLTester {
	public static void main (String[] args) {
		System.out.println("Tester");
		DigraphHL digrafo = new DigraphHL();
		
		digrafo.add(new Node("Hola"));
		digrafo.add(new Node("soy"));
		digrafo.add(new Node("una"));
		digrafo.add(new Node("secuencia"));
		digrafo.add(new Node("de"));
		digrafo.add(new Node("strings"));
		
		digrafo.add(new Edge("Hola","soy"));
		digrafo.add(new Edge("soy","una"));
		digrafo.add(new Edge("una","secuencia"));
		digrafo.add(new Edge("secuencia","de"));
		digrafo.add(new Edge("de","strings"));
		
		System.out.print(digrafo.toString());
		System.out.println("\nFin Tester");
	}
}
