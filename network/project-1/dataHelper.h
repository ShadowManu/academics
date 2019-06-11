/*
 * Archivo: dataHelper.c
 * Descripcion:
 * 		Provee funciones para ser usadas por las aplicaciones
 * 		cliente y servidor, simplificando y estandarizando su implementacion
 *
 * Fecha: Nov, 2013
 *
 * Autores:
 * 		Manuel Pacheco | 10-10524 | manuelalejandropm@gmail.com
 * 		Cristian Medina | 10-10445 | cmedina270793@gmail.com
 */

#ifndef DATAHELPER_H_
#define DATAHELPER_H_

#define NAMES_SIZE 50
#define CMD_SIZE 3
#define CMD_MAX_SIZE 1024

#define MSG_CODE_QTY 8

#define MSG_CODE_SHUT 0
#define MSG_CODE_SAL 1
#define MSG_CODE_USU 2
#define MSG_CODE_MEN 3
#define MSG_CODE_SUS 4
#define MSG_CODE_DES 5
#define MSG_CODE_CRE 6
#define MSG_CODE_ELI 7
#define MSG_CODE_FUE 8

#define MSG_CODE_SUS_NOROOM 9
#define MSG_CODE_SUS_ALREADY 10

#define MSG_CODE_DES_NOROOM 13

#define MSG_CODE_CRE_ALREADY 14

#define MSG_CODE_USRNAME 16

#define DEFAULT_PORT 20524

typedef struct {
	int code;
	int msgSize;
} msgHeader;

/*
 * Elimina caracteres /r y /n de un string
 */
void cleanString(char *str);

/*
 * Concatena a un string 'a' de buffer asignado 'aSize'
 * un string 'b' aumentando el buffer si es necesario
 */
void addString(char **a, int *aSize, char *b);

/*
 * Lee de un file descriptor 'fd' hasta leer 'size' bytes
 * escribiendo el resultado en 'buffer'
 */
void readBytes(int fd, void *buffer, int size);

/*
 * Lee de un 'buffer' y escribe en un file descriptor 'fd'
 * un numero 'size' de bytes
 */
void writeBytes(int fd, void *buffer, int size);

#endif /* DATAHELPER_H_ */
