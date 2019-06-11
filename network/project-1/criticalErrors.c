/*
 * Archivo: criticalErrors.c
 * Descripcion:
 * 		Maneja los errores que evitan la continuacion
 * 		del programa
 *
 * Fecha: Nov, 2013
 *
 * Autores:
 * 		Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 * 		Cristian Medina | 10-10445 | cmedina270793@gmail.com
 */

#include <stdio.h>
#include <stdlib.h>
#include "criticalErrors.h"

void criticalError(int code) {
	// #! No ha sido implementado todavia
	printf("Critical ERROR %d\n",code);
	exit(EXIT_FAILURE);
}

/* CRITICALERRORS_C_ */
