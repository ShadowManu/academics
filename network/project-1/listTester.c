/*
 * Archivo: listTester.c
 * Descripcion:
 * 		Prueba las operaciones de la lista generica
 *
 * Fecha: Nov, 2013
 *
 * Autores:
 * 		Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 * 		Cristian Medina | 10-10445 | cmedina270793@gmail.com
 */

#include <stdio.h>
#include "list.h"

void list_int_print(list *list) {
	listIterator iter;
	int *intp, count;

	printf("La lista tiene los elementos:\n");

	listIterator_create(list,&iter);
	count = 0;

	while (listIterator_hasNext(&iter)) {
		intp = (int*) listIterator_next(&iter);
		printf("Elemento %d es %d\n",count,*intp);
		++count;
	}
	printf("\n");
}

int main() {
	list myList;
	listIterator myIter;
	int count;

	// Crear lista
	list_new(&myList,sizeof(int));

	// Agregar elementos
	count = 10;
	list_append(&myList,(void*) &count);
	count = 12;
	list_append(&myList,(void*) &count);
	count = 34;
	list_append(&myList,(void*) &count);
	count = 6;
	list_append(&myList,(void*) &count);
	count = 18;
	list_append(&myList,(void*) &count);
	count = 37;
	list_append(&myList,(void*) &count);
	count = 1;
	list_append(&myList,(void*) &count);
	count = 42;
	list_append(&myList,(void*) &count);
	count = 25;
	list_append(&myList,(void*) &count);

	// Revisar tamano e imprimir
	printf("El tamano de la lista es %d\n", list_getSize(&myList));
	list_int_print(&myList);

	// Obtener iterador y eliminar elementos pares
	listIterator_create(&myList,&myIter);

	count = 0;
	while (listIterator_hasNext(&myIter)) {
		listIterator_next(&myIter);
		if (count % 2) {
			listIterator_unlink(&myIter);
		}
		++count;
	}

	// Revisar tamano e imprimir
	printf("El tamano de la lista ahora es %d\n", list_getSize(&myList));
	list_int_print(&myList);

	// Obtener nuevo iterador y eliminar todos los elementos
	listIterator_create(&myList,&myIter);

	while (listIterator_hasNext(&myIter)) {
		listIterator_next(&myIter);
		listIterator_unlink(&myIter);
	}

	// Revisar tamano e imprimir
	printf("El tamano de la lista ahora es %d\n", list_getSize(&myList));
	list_int_print(&myList);

	// Agregar un par de elementos y destruir lista
	count = 18;
	list_append(&myList,(void*) &count);
	count = 37;
	list_append(&myList,(void*) &count);
	list_destroy(&myList);

	// Revisar tamano e imprimir
	printf("El tamano de la lista ahora es %d\n", list_getSize(&myList));
	list_int_print(&myList);

	return 0;
}

/* LISTTESTER_C_ */
