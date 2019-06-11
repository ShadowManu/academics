/*
 * Archivo: dataHelper.c
 * Descripcion:
 * 		Implementacion de las estructuras
 *
 * Fecha: Nov, 2013
 *
 * Autores:
 * 		Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 * 		Cristian Medina | 10-10445 | cmedina270793@gmail.com
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "criticalErrors.h"

void cleanString(char *str) {
	int pos = strlen(str)-1;
	while (pos >= 0) {
		if (*(str + pos) == '\r' ||
			*(str + pos) == '\n')
			*(str+pos) = '\0';
		else break;
	}
}

void addString(char **a, int *aSize, char *b) {
	int newSize;

	if (b == NULL) return;
	newSize = strlen(b) + 1;
	if (*aSize > 0) newSize += strlen(*a);

	// Buffer excedido
	if (newSize > *aSize) {
		// Alojar nuevo espacio en el buffer
		if ((*a = realloc(*a,*aSize = (newSize << 1))) == NULL) {
			criticalError(HELPER_REALLOC_BUFFER);
		}
	}

	// Concatenar strings
	strcat(*a,b);
	return;
}

void readBytes(int fd, void *buffer, int size) {
	int bytesRead = 0;
	int result;

	while (bytesRead < size) {
		result = read(fd, buffer+bytesRead, size-bytesRead);
		if (result < 1)	criticalError(HELPER_READ_FD);
		bytesRead += result;
	}
}

void writeBytes(int fd, void *buffer, int size) {
	int bytesWrite = 0;
	int result;

	while (bytesWrite < size) {
		result = write(fd, buffer+bytesWrite, size-bytesWrite);
		if (result < 1)	criticalError(HELPER_WRITE_FD);
		bytesWrite += result;
	}
}

/* DATAHELPER_C_ */
