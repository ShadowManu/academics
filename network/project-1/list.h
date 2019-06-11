/*
 * Archivo: list.h
 * Descripcion:
 * 		Define las estructuras de datos y funciones que pueden ser
 * 		usadas para el uso de listas e iteradores genericos. Requiere
 * 		que la estructuras que se agregan sean totalmente estaticas (no
 * 		se libera memoria que no haya sido reservada directamente
 * 		por las operaciones de lista)
 *
 * Fecha: Nov, 2013
 *
 * Autores:
 * 		Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 * 		Cristian Medina | 10-10445 | cmedina270793@gmail.com
 */

#ifndef list_H_
#define list_H_

typedef struct _listNode {
	void *data;
	struct _listNode *next;
} listNode;

typedef struct {
	int length;
	int elemSize;
	listNode *head;
	listNode *tail;
} list;

typedef struct {
	list *list;
	listNode *next;
	listNode *last;
	listNode *last2;
} listIterator;

/*
 * Inicializa los miembros de una lista para ser usada
 */
void list_new(list *list, int elementSize);

/*
 * Destruye los elementos y queda como nueva
 */
void list_destroy(list *list);

/*
 * Agrega una copia en memoria del elemento al final de la lista
 */
void list_append(list *list, void *element);

/*
 * Retorna el tamano de la lista
 */
int list_getSize(list *list);

/*
 * Inicializa un iterador sobre una lista
 */
void listIterator_create(list *list, listIterator *iter);

/*
 * Devuelve si hay o no un siguiente elemento en la lista
 */
int listIterator_hasNext(listIterator *iter);

/*
 * Devuelve un apuntador al espacio donde esta el elemento
 * y mueve el iterador
 */
void *listIterator_next(listIterator *iter);

/*
 * Elimina el ultimo elemento retornado por next()
 * liberando tambien su memoria ocupada
 */
void listIterator_unlink(listIterator *iter);

#endif /* LIST_H_ */
