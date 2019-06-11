/*
 * Archivo: list.h
 * Descripcion:
 * 		Implementacion de lista generica e iterador sobre lista
 * 		estilo Java.
 *
 * Fecha: Nov, 2013
 *
 * Autores:
 * 		Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 * 		Cristian Medina | 10-10445 | cmedina270793@gmail.com
 */

#include <stdlib.h>
#include <string.h>

#include "list.h"
#include "criticalErrors.h"

void list_new(list *list, int elementSize) {
	list->length = 0;
	list->elemSize = elementSize;
	list->head = list->tail = NULL;
}

void list_destroy(list *list) {
	listNode *current;

	while(list->head != NULL) {
		current = list->head;
		list->head = current->next;

		// Liberar el espacio del objeto y el nodo
		free(current->data);
		free(current);
	}
	list->length = 0;
}

void list_append(list *list, void *element) {
	// Se hace un nodo
	listNode *node = malloc(sizeof(listNode));
	if (node == NULL) criticalError(LIST_MALLOC_NODE);
	node->data = malloc(list->elemSize);
	if (node->data == NULL) criticalError(LIST_MALLOC_DATA);
	node->next = NULL;

	// Se copia el elemento en el nodo
	memcpy(node->data, element, list->elemSize);

	// Si no hay elementos en la lista
	if (list->length == 0) {
		list->head = list->tail = node;
	} else {
		list->tail->next = node;
		list->tail = node;
	}
	++(list->length);
}

int list_getSize(list *list) {
	return list->length;
}

void listIterator_create(list *list, listIterator *iter) {
	iter->list = list;
	iter->next = list->head;
	iter->last = NULL;
	iter->last2 = NULL;
}

int listIterator_hasNext(listIterator *iter) {
	return iter->next != NULL;
}

void *listIterator_next(listIterator *iter) {
	if (iter->last != NULL) iter->last2 = iter->last;
	iter->last = iter->next;
	iter->next = iter->next->next;
	return iter->last->data;
}

void listIterator_unlink(listIterator *iter) {
	if (iter->last == NULL) return;

	// Si es el primer elemento
	if (iter->last2 == NULL) {
		// Nuevo head
		iter->list->head = iter->next;

	// O es en cualquier otra posicion
	} else {
		iter->last2->next = iter->next;
	}

	// De cualquier forma
	// Borrar data y nodo
	free(iter->last->data);
	free(iter->last);
	iter->last = NULL;

	// Si por otra parte es el ultimo elemento
	if (iter->next == NULL) {
		iter->list->tail = iter->last2;
		// No hay problema que last2 sea null
		// ya que entonces la lista queda vacia
	}

	// Ajustar el nuevo tamano
	--iter->list->length;
}

/* LIST_C_ */
