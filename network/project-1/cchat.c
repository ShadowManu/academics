/**
 * Archivo: cchat.c
 * Descripcion:
 * 		Cliente de un programa de chat.
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

#include <pthread.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h> 

#include "dataHelper.h"
#include "criticalErrors.h"
#include "optionHandler.h"

/*
 * Argumento para el thread de lectura
 */
typedef struct {
	int sockfd;
	char **outMainPtr;
	int *outMainSizePtr;
} threadStruct;

/*
 * Usado para la coordinacion entre los hilos para accesar
 * al buffer del output
 */
//pthread_mutex_t outputLock = PTHREAD_MUTEX_INITIALIZER;

/* 
 * Retorna el codigo del comando asociado. Si no es un comando
 * valido, retorna -1
 */
int parseCommand(char *str) {
	unsigned int i;
	char *commands[] = {
			"Brace yourselves shutdown is coming",
			"sal",
			"usu",
			"men ",
			"sus ",
			"des",
			"cre ",
			"eli ",
			"fue",
	};

	for (i = 0; i < (sizeof(commands)/sizeof(commands[0])); ++i) {
		// #! Si strncomp no es seguro con el tamano, agregar check aqui
		if (strncmp(str,commands[i],strlen(commands[i])) == 0) {
			return i;
		}
	}
	return -1;
}

/*
 * Maneja un string que contiene el comando
 * que se envia al servidor. Devuelve 0 si la ejecucion es exitosa y
 * -1 si el comando no es valido.
 */
int handleCommandMsg(int sockfd, char* str) {
	int code;
	msgHeader header;

	// Obtener codigo (y que sea legal)
	code = parseCommand(str);
	if (code < 0) return -1;

	switch (code) {

	case MSG_CODE_SHUT:
	case MSG_CODE_SAL:
	case MSG_CODE_USU:
	case MSG_CODE_FUE:
	case MSG_CODE_DES:

		// Enviar solo el header
		header.code = code;
		header.msgSize = 0;
		writeBytes(sockfd,(void *) &header,sizeof(header));
		break;

	case MSG_CODE_MEN:
	case MSG_CODE_SUS:
	case MSG_CODE_CRE:
	case MSG_CODE_ELI:

		// Enviar header y mensaje
		header.code = code;
		header.msgSize = strlen(str + 4) + 1;
		writeBytes(sockfd,(void *) &header,sizeof(header));
		writeBytes(sockfd,(void *) str+4, header.msgSize);
		break;
	}
	return 0;
}

void readFile(int sockfd, char *fileName) {
	FILE *file;
	char *str;
	size_t strAllocSize;
	int numChars;

	// Abrir el archivo
	if ((file = fopen(fileName,"r")) == NULL) {
		criticalError(CL_READFILE_OPEN);
	}

	// Leer las lineas y por cada una
	while ((numChars = getline(&str,&strAllocSize,file)) > 0) {

		// Manejar el comando
		if (handleCommandMsg(sockfd,str) != 0) {
			printf("El comando introducido no es valido");
			continue;
		}
	}

	// Liberar el buffer
	free(str);

	// En caso de error en lectura
	if (numChars < 0) {
		criticalError(CL_READFILE_GETLINE);
	}

	fclose(file);
}

void *responseHandler(void *arg) {
	threadStruct *inArg = (threadStruct *) arg;		// Temp para procesar args
	int sockfd = inArg->sockfd;						// Socket file descriptor
	char **outMainPP = inArg->outMainPtr;			// Dir de buffer de salida
	int *outMainSizePP = inArg->outMainSizePtr;		// Dir de tamano de buffer
	free(inArg);

	msgHeader header;									// Header de mensaje
	char *outMsg = malloc(sizeof(char)*CMD_MAX_SIZE);	// Buffer por mensaje
	bzero((void *)outMsg,CMD_MAX_SIZE);

	while (1) {
		readBytes(sockfd,&header,sizeof(header));
		readBytes(sockfd,outMsg,header.msgSize);

		//if (pthread_mutex_lock(&outputLock)) criticalError(CL_LOCK);

		// Agregar un string dependiendo del tipo de respuesta
		switch (header.code) {

		case MSG_CODE_SHUT:
			addString(outMainPP,outMainSizePP,
					"EL SERVIDOR VA A CERRARSE\n");
			_exit(EXIT_SUCCESS);
			break;

		case MSG_CODE_SAL:
			addString(outMainPP,outMainSizePP,
					"Las salas en el servidor son:\n");
			break;

		case MSG_CODE_USU:
			addString(outMainPP,outMainSizePP,
					"Los usuarios en el servidor son:\n");
			break;

		case MSG_CODE_MEN:
			addString(outMainPP,outMainSizePP,
					"\n");
			break;

		case MSG_CODE_SUS:
			addString(outMainPP,outMainSizePP,
					"Te has suscrito correctamente a la sala\n");
			break;

		case MSG_CODE_DES:
			addString(outMainPP,outMainSizePP,
					"Te has desuscrito correctamente de todas las salas\n");
			break;

		case MSG_CODE_CRE:
			addString(outMainPP,outMainSizePP,
					"Se ha creado la sala con exito\n");
			break;

		case MSG_CODE_ELI:
			addString(outMainPP,outMainSizePP,
					"Se ha eliminado la sala con exito\n");
			break;

		case MSG_CODE_FUE:
			printf("Saliendo del programa. Presione enter para terminar de salir.\n");
			close(sockfd);
			*outMainSizePP = -1;
			exit(EXIT_SUCCESS);
			break;

		case MSG_CODE_SUS_NOROOM:
			addString(outMainPP,outMainSizePP,
					"La sala indicada no existe\n");
			break;

		case MSG_CODE_SUS_ALREADY:
			addString(outMainPP,outMainSizePP,
					"Ya estas suscrito a la sala\n");
			break;

		case MSG_CODE_DES_NOROOM:
			addString(outMainPP,outMainSizePP,
					"No estabas suscrito a ninguna sala\n");
			break;

		case MSG_CODE_CRE_ALREADY:
			addString(outMainPP,outMainSizePP,
					"La sala ya existe\n");
			break;
		}

		// Agregar string de mensaje
		addString(outMainPP,outMainSizePP,outMsg);

		printf(*outMainPP);

		//if (pthread_mutex_unlock(&outputLock)) criticalError(CL_UNLOCK);

		bzero((void *) *outMainPP,*outMainSizePP);
		bzero((void *)outMsg,CMD_MAX_SIZE);
	}
	return NULL;
}

int main(int argc, char *argv[]){
	char *hostAddress = "localhost";		// Direccion del host
	char *portNum = "20524";				// Puerto de conexion
	char *userName = argv[1];				// Nombre de usuario
	char *fileName = NULL;					// Nombre de archivo
	
	int sockfd;								// Socket de conexion
	msgHeader header;						// Header para enviar el Usuario

	struct addrinfo hints;					// Criterio para direciones
	struct addrinfo *result, *rp;			// Info de direcciones posibles
	int status;								// Status de la busqueda

	char *str = NULL;						// Buffer de input del cliente
	size_t strAllocSize;					// Tamano del buffer
	int numChars;							// Caracteres leidos en una entrada

	pthread_t pthread;
	threadStruct *threadArg;				// Argumento para el hilo lector
	char *outMain;							// Buffer de mensajes recibidos
	int outMainSize;						// Tamano del buffer

	// Setear argumentos
	clOptionHandler(argc,argv,&hostAddress,&portNum,&userName,&fileName);

	// Setear criteria de busqueda de direcciones
	memset(&hints, 0, sizeof(struct addrinfo));		// Limpiar hints
	hints.ai_family = AF_INET;						// Familia Internet
	hints.ai_socktype = SOCK_STREAM; 				// Orientado a conexion
	hints.ai_flags = 0;								// Nada en especial
	hints.ai_protocol = 0;			 				// Sera TCP

	// Obtener direcciones
	if ((status = getaddrinfo(hostAddress, portNum, &hints, &result)) != 0) {
		fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(status));
		exit(EXIT_FAILURE);
	}

	// getaddrinfo() devuelve una lista de estructras (de direcciones).
	// Intentar conectarse reintentando si socket() o connect() falla
	for (rp = result; rp != NULL; rp = rp->ai_next) {

		// Crear socket
		sockfd = socket(rp->ai_family,
						rp->ai_socktype,
						rp->ai_protocol);
		if (sockfd == -1) continue;			// La creacion falla

		// Conectar socket
		if (connect(sockfd,
					rp->ai_addr,
					rp->ai_addrlen) != -1) {
			break;							// La coneccion es exitosa
		}

		close(sockfd);						// Si la conexion falla
											// cerrar el socket
	}

	// No se pudo obtener una direccion valida
	if (rp == NULL) criticalError(CL_CONNECT);

	// Vaciar la lista (ya no es necesaria)
	freeaddrinfo(result);
	
	// Se envia el nombre del usuario
	header.code = MSG_CODE_USRNAME;
	header.msgSize = strlen(userName) + 1;
	writeBytes(sockfd,(void *) &header,sizeof(header));
	writeBytes(sockfd,(void *) userName,header.msgSize);

	// Crear el thread de lectura de respuestas del servidor
	outMain = malloc(sizeof(char)); *outMain = '\0';
	outMainSize = 0;

	threadArg = malloc(sizeof(threadStruct));
	threadArg->sockfd = sockfd;
	threadArg->outMainPtr = &outMain;
	threadArg->outMainSizePtr = &outMainSize;

	pthread_create(&pthread,NULL,responseHandler,threadArg);

	// Leer el archivo si es dado
	//if (strcmp(fileName,"") != 0) readFile(sockfd,fileName);

	while (1) {

		// Recibir una linea
		numChars = getline(&str,&strAllocSize,stdin);
		if (outMainSize == -1) exit(EXIT_SUCCESS);
		
		if (numChars < 0) {
			free(str);
			criticalError(CL_MSGINPUT);
		}

		// Manejar el comando
		if (handleCommandMsg(sockfd,str) != 0) {
			printf("El comando introducido no es valido\n");
			continue;
		}

		// Imprimir y limpiar los mensajes pendientes
		//if (pthread_mutex_lock(&outputLock)) criticalError(CL_LOCK);

		//if (pthread_mutex_unlock(&outputLock)) criticalError(CL_UNLOCK);

	}

	return 0;
}
