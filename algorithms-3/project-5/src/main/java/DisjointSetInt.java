
public class DisjointSetInt {

	private int[] parent;
	private int[] rank;

	public DisjointSetInt(int nElems) {
		this.parent = new int[nElems];
		this.rank = new int[nElems];

		// Create new elems
		for (int i = nElems - 1; i >= 0; --i) {
			this.parent[i] = i;
			this.rank[i] = 0;
		}
	}

	public int find(int x) {
		if (parent[x] == x) {
			return x;
		} else {
			// Optimization: Path Compression
			parent[x] = find(parent[x]);
		}
		return parent[x];
	}

	public void union(int a, int b) {
		int aRoot = find(a);
		int bRoot = find(b);

		if (aRoot == bRoot) { // Same set already
			return;
		}

		// Optimization: Union by rank
		if (rank[aRoot] > rank[bRoot]) { // A in-tree may be deeper
			parent[bRoot] = aRoot;
		} else if (rank[aRoot] < rank[bRoot]) { // B in-tree may be deeper
			parent[aRoot] = bRoot;
		} else { // Same size
			parent[bRoot] = aRoot;
			++rank[aRoot];
		}
	}
}
