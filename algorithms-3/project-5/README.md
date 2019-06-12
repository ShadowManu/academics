# Project 5

A Kruskal-based implementation of finding the minimum spanning forest (particularly, the size of the biggest edge of the forest).

The implementation is based on `List` for edges and `DisjointSetInt` for edges in forests.

Run it with `docker-compose build && docker-compose run program`

You can change the program input in `in.out`. It has the following format:

```
<number of cases>
<how many splits are allowed (forests - 1)> <number of vertices>
<vertex1 x> <vertex1 y>
<vertex2 x> <vertex2 y>
...
<vertexN x> <vertexN y>
```

Originally authored in January, 2014.
