/*
 * Archivo: criticalErrors.h
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

#ifndef CRITICALERRORS_H_
#define CRITICALERRORS_H_

#define LIST_MALLOC_NODE 1
#define LIST_MALLOC_DATA 2

#define HELPER_REALLOC_BUFFER 3
#define HELPER_READ_FD 4
#define HELPER_WRITE_FD 5

#define SV_SOCKET 6
#define SV_BIND 7
#define SV_LISTEN 8
#define SV_ACCEPT 9

#define CL_READFILE_OPEN 10
#define CL_READFILE_GETLINE 11

#define CL_CONNECT 12
#define CL_MSGINPUT 13

#define CL_LOCK 14
#define CL_UNLOCK 15

#define CL_ARGS 16
#define SV_ARGS 17

void criticalError(int code);

#endif /* CRITICALERRORS_H_ */
