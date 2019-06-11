/**
 * Clase concreta de DiGraph
 * Representado bajo un modelo de tablas de hash
 * con manejo de colisiones con listas enlazadas simples
 * de nodos (con el uso de MyList<Node>)
 *
 */
public class DigraphHL extends Digraph{

	private HashList table;
    /*
     * Construye un grafo vacio.
     */
    public DigraphHL() {
    	super();
    	table = new HashList();
    }

    /**
     * Agrega la arista dada al grafo. Si los vertices de la arista
     * no existen o el grafo tiene una arista entre dichos vertices,
     * retorna false. Si se agrega la nueva arista, retorna true.
     * 
     * Complejidad: O(p + q), p << | V | y q << | E |
     */
    public boolean add(Edge e){
    	Node bSrc = this.table.get(new Node(e.getSrc()));
    	Node bDst = this.table.get(new Node(e.getDst()));
    	if ((bDst!=null)&&(bSrc!=null)){
    		bSrc.addOutEdge(e);
    		bDst.addInEdge(e);
    		this.numEdges++;
    		return true;
    	}
    	return false;
    }

    /**
     * Agrega el nodo n. Si el vertice ya existe, retorna false. Si
     * se agrega el nodo, retorna true.
     *
     * Complejidad: O(p), p << | V |
     */
    public boolean add(Node n) {
    	if (table.add(n) == true) {
    		++numVertices;
    		return true;
    	}
    	return false;
    }

    /**
     * Elimina los nodos y aristas del grafo.
     * 
     * Complejidad: O(1)
     */
    public void clear(){
    	this.table = new HashList();
    	this.numEdges=0;
    	this.numVertices=0;
    }

    /*
     * Retorna un nuevo grafo que es una copia del grafo actual.
     * 
     * Complejidad: O(|V| + |E|)
     */
    public Object clone(){
    	throw new UnsupportedOperationException("No se ha implementado este metodo");
		/*
    	DigraphHL newGraph = new DigraphHL();
		newGraph.numEdges = this.numEdges;
		newGraph.numVertices = this.numVertices;
		newGraph.table = this.table.clone();
		return newGraph;
		*/
    }

    /**
     * Chequa que el grafo contiene una arista (src, dst).
     * 
     * Complejidad: O(p), p << | E |
     */
    public boolean contains(String src, String dst){
    	Node auxB = this.table.get(new Node(src));
    	if (auxB!=null){
    		return auxB.getOE().contains(new Edge(src,dst));
    	}
    	return false;
    }

    /**
     * Chequa que el grafo contiene una nodo con id nod
     *
     * Complejidad: O(p), p << | V |
     */
    public boolean contains(String nod) {
    	Node auxB = this.table.get(new Node(nod));
    	if (auxB!=null){
    		return true;
    	}
    	return false;
    }

    /**
     * Retorna la arista del grafo que conecta a los vertices
     * src y dst. Si no existe dicha arista, retorna null.
     * 
     *  Complejidad: O(p), p << | E |
     */
    public Edge get(String src, String dst){
    	Node auxB = this.table.get(new Node(src));
    	if (auxB!=null){
    		return auxB.getOE().get(new Edge(src,dst));
    	}
    	return null;
    }

    /*
     * Retorna todas las aristas del grafo
     *
     * Complejidad: O(???)
     */
    public List<Edge> getEdges() {
    	MyList<Edge> out = new MyList<Edge>();
    	HashListIterator iter = table.iterator();
    	
    	// Por cada lista de nodos
    	while (iter.hasNext()) {
    		ListIterator<Node> listIter = iter.next().iterator();
    		
    		// En cada nodo
    		while (listIter.hasNext()) {
    			ListIterator<Edge> internalIter = listIter.next().getIE().iterator();
    			
    			// Por cada vertice en el nodo
    			while (internalIter.hasNext()) {
    				out.add(internalIter.next());
    			}
    		}
    	}
    	return out;
    }

    /*
     * Retorna el nodo con id nod. Si no existe dicho nodo, 
     * retorna null.
     *
     * Complejidad: O(p), p << |V|
     */
    public Node get(String nod){
    	return this.table.get(new Node(nod));
    }
    /* 
     * Retorna todos los nodos del grafo.
     *
     * Complejidad: O(|Vertices|)
     */
    public List<Node> getNodes() {
    	MyList<Node> out = new MyList<Node>();
    	HashListIterator iter = table.iterator();
    	
    	// Por cada lista de nodos
    	while (iter.hasNext()) {
    		ListIterator<Node> listIter = iter.next().iterator();
    		
    		// En cada nodo
    		while (listIter.hasNext()) {
    			out.add(listIter.next());
    		}
    	}
    	return out;
    }

    /**
     * Retorna el numero de aristas en el grafo.
     *
     * Complejidad: O(1)
     */
    public int getNumEdges() {
        return numEdges;
    }

    /**
     * Retorna el numero de vertices en el grafo.
     * 
     * Complejidad: O(1)
     */
    public int getNumVertices() {
        return numVertices;
    }

    /**
     * Retorna la lista de lados que tienen al vertice dado como
     * destino. Si el vertice no existe, retorna null.
     * 
     * Complejidad: O(p), p << | E |
     */
    public List<Edge> getInEdges(String node) {
    	Node auxB = this.table.get(new Node(node));
    	if (auxB!=null){
    		return auxB.getIE();
    	}
    	return null;
    }

    /**
     * Retorna los predecesores del nodo con id node
     * 
     * Complejidad: O(p), p << | E |
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public  List<Node> getPreds(String node){
	if (!contains(node))
	    return null;

	List<Edge> inEdges = getInEdges(node);

	if (inEdges == null)
	    return null;

	ListIterator<Edge> li = inEdges.iterator();
	List<Node> preds = (List<Node>) new MyList();

	while (li.hasNext()) {
	    Edge e = li.next();
	    String sid = e.getSrc();

	    preds.add(this.get(sid));
	}
	    
	return preds;
    }

    /**
     * Retorna la lista de lados que tienen al vertice dado como
     * origen. Si el vertice no existe, retorna null.
     *
     * Complejidad: O(p), p << | E |
     */
    public  List<Edge> getOutEdges(String node){
    	Node auxB = this.table.get(new Node(node));
    	if (auxB!=null){
    		return auxB.getOE();
    	}
    	return null;

    }

    /**
     * Retorna los sucesores del nodo con id node
     *
     * Complejidad: O(p), p << | E |
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public  List<Node> getSucs(String node){
	if (!contains(node))
	    return null;

	List<Edge> outEdges = getOutEdges(node);

	if (outEdges == null)
	    return null;

	ListIterator<Edge> li = outEdges.iterator();
	List<Node> sucs = new MyList();

	while (li.hasNext()) {
	    Edge e = li.next();
	    String sid = e.getDst();

	    sucs.add(this.get(sid));
	}
	    
	return sucs;
    }

    /**
     * Retorna el in-degree del vertice dado. Si el
     * vertice no existe, retorna -1.
     *
     * Complejidad: O(p), p << | E |
     */
    public int getInDegree(String node)  {
    	Node auxB = this.table.get(new Node(node));
    	if (auxB!=null){
    		return auxB.getIE().getSize();
    	}
    	return -1;
    }
    /**
     * Retorna el out-degree del vertice dado. Si el
     * vertice no existe, retorna -1.
     *
     * Complejidad: O(p), p << | E |
     */
    public int getOutDegree(String node) {
    	Node auxB = this.table.get(new Node(node));
    	if (auxB!=null){
    		return auxB.getOE().getSize();
    	}
    	return -1;
    }

    /**
     * Remueve la arista del grafo que conecta a los vertices src y
     * dst. Si el grafo no cambia, retorna false. Si el grafo cambia,
     * retorna true.
     * 
     * Complejidad: O(p), p << | E |
     */
    public boolean remove(String src, String dst){
    	Node auxB1 = this.table.get(new Node(src));
    	Node auxB2 = this.table.get(new Node(dst));
    	if ((auxB1!=null)&&(auxB2!=null)){
    		auxB1.getOE().remove(new Edge(src,dst));
    		auxB2.getIE().remove(new Edge(src,dst));
    		return true;
    	}
    	return false;
    }

    /*
     * Remueve el nodo del grafo con id nod. Si el grafo no cambia,
     * retorna false. Si el grafo cambia, retorna true.
     *
     * Complejidad: O(p), p << | V |
     */
    public boolean remove(String nod){
    	Node auxB = this.table.get(new Node(nod));
    	Node auxB2; 
    	if (auxB!=null){
    		ListIterator<Edge> edgs =auxB.getIE().iterator(); 
    		while (edgs.hasNext()){}
    			auxB2 = this.table.get(new Node(edgs.next().getSrc()));
    			auxB2.getOE().remove(new Edge(auxB2.toString(),auxB.toString()));
    		
    		edgs = auxB.getOE().iterator(); 
    		while (edgs.hasNext()){
    			auxB2 = this.table.get(new Node(edgs.next().getDst()));
    			auxB2.getIE().remove(new Edge(auxB2.toString(),auxB2.toString()));
    		}
    		return true;
    	}
    	return false;
    }

    /**
     * Construye una representacion en String del grafo.
     */
    public String toString() {
        String ret = numVertices + ":" + numEdges ;

		ListIterator<Node> nods = getNodes().iterator();
		
		while (nods.hasNext()) {
		    Node n = nods.next();
		    ret += "\n" + n.toString();
		}       
	
		ListIterator<Edge> edgs = getEdges().iterator();
	
		while (edgs.hasNext()) {
		    Edge e = edgs.next();
		    ret += "\n" + e.toString();
		}       
	
	    return ret;
    }
}

// End Digraph.java
