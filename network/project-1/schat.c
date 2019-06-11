/**
 * Archivo: schat.c
 * Descripcion:
 * 		Servidor de un programa de chat.
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

#include "list.h"
#include "dataHelper.h"
#include "criticalErrors.h"
#include "optionHandler.h"

#define NUM_CONNECTIONS 5

/*
 * Argumento para los threads
 */
typedef struct {
	int sockfd;
	char *userName;
} threadStruct;

/*
 * Tipo sala
 */
typedef struct {
	list users;
	char name[NAMES_SIZE];
} Room;

/*
 * Tipo usuario
 */
typedef struct {
	char name[NAMES_SIZE];
	int sockfd;
} User;

/*
 * Se define el apuntador a la estructura de datos como variables globales
 * para reducir la sintaxis de las operaciones del servidor. Esto es debido
 * a que todas las operaciones de los clientes requieren acceso a estas
 * estructuras.
 */
list Users;
list Rooms;

/*
 * Procesa el comando sal: escribe el nombre
 * de las salas que existen en el servidor.
 */
void sal(int sockfd) {
	char *roomName;					// Nombre de una sala en particular
	listIterator roomIter;			// Iterador para las salas

	char *answer = NULL;			// Buffer del mensaje
	int answerSize = 0;				// Tamano del buffer
	msgHeader header;				// Header del mensaje

	// Por cada sala
	listIterator_create(&Rooms, &roomIter);
	while (listIterator_hasNext(&roomIter)) {

		// Obtener y agregar a la respuesta el nombre de la sala
		roomName = ((Room *) listIterator_next(&roomIter))->name;
		addString(&answer,&answerSize,roomName);
		addString(&answer,&answerSize,"\n");
	}

	// Enviar Header y Mensaje
	header.code = MSG_CODE_SAL;
	header.msgSize = strlen(answer) + 1;
	writeBytes(sockfd,(void *) &header,sizeof(header));
	writeBytes(sockfd,(void *)answer,header.msgSize);
}

/*
 * Procesa el comando usu: escribe el nombre de los usuarios
 * que existen en el servidor.
 */
void usu(int sockfd) {
	char *userName;					// Nombre de una usuario en particular
	listIterator userIter;			// Iterador para los usuarios

	char *answer = NULL;			// Buffer del mensaje
	int answerSize = 0;				// Tamano del buffer
	msgHeader header;				// Header del mensaje

	// Por cada usuario
	listIterator_create(&Users, &userIter);
	while(listIterator_hasNext(&userIter)) {

		// Obtener y agregar a la respuesta el nombre de la sala
		userName = ((User *) listIterator_next(&userIter))->name;
		addString(&answer,&answerSize,userName);
		addString(&answer,&answerSize,"\n");
	}

	// Enviar Header y Mensaje
	header.code = MSG_CODE_USU;
	header.msgSize = strlen(answer) + 1;
	writeBytes(sockfd,(void *) &header,sizeof(header));
	writeBytes(sockfd,(void *)answer,header.msgSize);
}

/*
 * Procesa el comando men: envia un mensaje a los usuarios suscritos
 * a una misma sala que el emisor del mensaje.
 */
void men(int sockfd, char* msg) {
	Room *roomPtr;					// Apuntador a sala
	User **userPtrPtr;				// Apuntador a *usuario
	listIterator roomIter;			// Iterador de sala
	listIterator userPtrIter;		// Iterador de *usuario

	int *fdPtr;						// Apuntador a fd
	list fdList;					// Lista de fds
	listIterator fdIter;			// Iterador de fd

	int found = 0;					// Flag de encontrar en lista
	msgHeader header;				// Header del mensaje

	// Crear la lista de fds a los cuales se enviara el mensaje
	list_new(&fdList,sizeof(int));

	// Por cada sala
	listIterator_create(&Rooms,&roomIter);
	while (listIterator_hasNext(&roomIter)) {

		found = 0;
		roomPtr = (Room *) listIterator_next(&roomIter);

		// Por su lista de usuarios
		listIterator_create(&(roomPtr->users),&userPtrIter);
		while (listIterator_hasNext(&userPtrIter)) {

			// Buscar si el usuario (por su id) se encuentra en la sala
			userPtrPtr = (User **) listIterator_next(&userPtrIter);
			if ((*userPtrPtr)->sockfd == sockfd) {
				found = 1;
				break;
			}
		}
		if (!found) continue;

		// De haberlo encontrado agregar de forma
		// unica los fd de los demas usuarios a la lista
		listIterator_create(&(roomPtr->users),&userPtrIter);
		while (listIterator_hasNext(&userPtrIter)) {

			found = 0;
			userPtrPtr = (User **) listIterator_next(&userPtrIter);

			// Revisar si el fd ya esta en la lista
			listIterator_create(&fdList,&fdIter);
			while (listIterator_hasNext(&fdIter)) {

				fdPtr = (int *) listIterator_next(&fdIter);
				if (*fdPtr == (*userPtrPtr)->sockfd) {
					found = 1;
					break;
				}
			}
			if (found) continue;

			// Agregar el fd a la lista
			list_append(&fdList,(void *) &(*userPtrPtr)->sockfd);
		}
		// Fin de busqueda de usuarios de una sala

	}
	// Fin de busqueda en las salas

	// Enviar el mensaje a todos los usuarios encontrados
	*(msg + strlen(msg) + 1) = '\0';
	*(msg + strlen(msg)) = '\n';
	header.code = MSG_CODE_MEN;
	header.msgSize = strlen(msg) + 1;

	listIterator_create(&fdList,&fdIter);
	while (listIterator_hasNext(&fdIter)) {
		fdPtr = (int *) listIterator_next(&fdIter);
		writeBytes(*fdPtr,(void *) &header,sizeof(header));
		writeBytes(*fdPtr,(void *) msg,header.msgSize);
	}
}

 /*
  * Procesa el comando sus: suscribe el usuario
  * a la sala dada
  */
void sus(int sockfd, char *roomName) {
	User *user;
	Room *room;
	listIterator userIter;
	listIterator roomIter;

	listIterator userPtrIter;
	int found = 0;
	msgHeader header;				// Header del mensaje


	// Buscar la estructura donde se guarda el usuario
	listIterator_create(&Users,&userIter);
	while (listIterator_hasNext(&userIter)) {
		user = (User *) listIterator_next(&userIter);
		if (user->sockfd == sockfd) break;
	}

	// Buscar la sala
	listIterator_create(&Rooms,&roomIter);
	while (listIterator_hasNext(&roomIter)) {
		room = (Room *) listIterator_next(&roomIter);
		if (strcmp(room->name,roomName) == 0) {
			found = 1;
			break;
		}
	}

	// En caso de no encontrarla, enviar un error
	if (!found) {
		header.code = MSG_CODE_SUS_NOROOM;
		header.msgSize = 0;
		writeBytes(sockfd,(void *) &header,sizeof(header));
		return;
	}
	found = 0;

	// Revisar que el usuario no este ya suscrito a la sala;
	listIterator_create(&(room->users),&userPtrIter);
	while (listIterator_hasNext(&userPtrIter)) {
		if ((*((User **) listIterator_next(&userPtrIter)))->sockfd == sockfd) {
			found = 1;
			break;
		}
	}

	// En caso de que ya este suscrito
	if (found) {
		header.code = MSG_CODE_SUS_ALREADY;
		header.msgSize = 0;
		writeBytes(sockfd,(void *) &header,sizeof(header));
		return;
	}

	// Ya podemos agregarlo correctamente
	list_append(&(room->users),&user);

	// Decirle al usuario
	header.code = MSG_CODE_SUS;
	header.msgSize = 0;
	writeBytes(sockfd,(void *) &header,sizeof(header));
}

/*
 * Procesa el comando des: de-suscribe a un usuario de todas las salas.
 */
void des(int sockfd) {
	Room *room;
	User *user;
	listIterator roomIter;
	listIterator userPtrIter;

	int found = 0;
	msgHeader header;

	// Por cada una de las listas de usuarios salas
	listIterator_create(&Rooms,&roomIter);
	while (listIterator_hasNext(&roomIter)) {

		room = (Room *) listIterator_next(&roomIter);

		// Revisar si esta el usuario y eliminarlo
		listIterator_create(&(room->users),&userPtrIter);
		while (listIterator_hasNext(&userPtrIter)) {

			user = *((User **) listIterator_next(&userPtrIter));
			if (user->sockfd == sockfd) {
				listIterator_unlink(&userPtrIter);
				found = 1;
				break;
			}
		}
	}

	// En caso de que no este en ninguna sala
	if (!found) {
		header.code = MSG_CODE_DES_NOROOM;
		header.msgSize = 0;
		writeBytes(sockfd,(void *) &header,sizeof(header));
		return;
	}

	// Se de-suscribio correctamente
	header.code = MSG_CODE_DES;
	header.msgSize = 0;
	writeBytes(sockfd,(void *) &header,sizeof(header));
}

/*
 * Procesa el comando cre: crea una sala en el servidor;
 */
void cre(int sockfd, char *roomName) {
	Room  *room;
	listIterator roomIter;
	int found = 0;
	Room newRoom;
	msgHeader header;
	int size;

	// Busco si la sala ya existe
	listIterator_create(&Rooms,&roomIter);
	while (listIterator_hasNext(&roomIter)) {
		room = (Room *) listIterator_next(&roomIter);
		if (strcmp(room->name,roomName) == 0) {
			found = 1;
			break;
		}
	}

	// En caso de que ya exista
	if (found) {
		header.code = MSG_CODE_CRE_ALREADY;
		header.msgSize = 0;
		writeBytes(sockfd,(void *) &header,sizeof(header));
		return;
	}

	// De lo contrario se agrega la sala
	strncpy(newRoom.name,roomName,NAMES_SIZE);
	*(newRoom.name + NAMES_SIZE - 1) = '\0';
	size = strlen(newRoom.name);
	if (*(newRoom.name + size - 1) == '\n')
		*(newRoom.name + size - 1) = '\0';
	list_new(&newRoom.users,sizeof(User *));
	list_append(&Rooms,(void *) &newRoom);

	// Se envia el mensaje de correctitud
	header.code = MSG_CODE_CRE;
	header.msgSize = 0;
	writeBytes(sockfd,(void *) &header,sizeof(header));
}

/*
 * Elimina un usuario del servidor
 */
void fue(int sockfd) {
	listIterator userIter;		// Iterador de usuarios
	msgHeader header;			// Mensaje respuesta

	// De-suscribir el usuario de las salas
	des(sockfd);

	// Eliminar el usuario de la lista de usuarios
	listIterator_create(&Users,&userIter);
	while (listIterator_hasNext(&userIter)) {
		if (((User *) listIterator_next(&userIter))->sockfd == sockfd) {
			listIterator_unlink(&userIter);
			break;
		}
	}

	// Enviar mensaje al cliente
	header.code = MSG_CODE_FUE;
	header.msgSize = 0;
	writeBytes(sockfd,&header,sizeof(header));
	close(sockfd);
}

void * clientHandler(void * arg) {
	threadStruct *inArg = (threadStruct *) arg;
	int sockfd = inArg->sockfd;
	free(inArg);

	char *msg = malloc(sizeof(char)*CMD_MAX_SIZE);
	*msg = '\0';
	msgHeader header;
	User user;

	// Agregar usuario
	readBytes(sockfd, (void *) &header,sizeof(header));
	readBytes(sockfd, (void *) msg, header.msgSize);
	cleanString(msg);

	user.sockfd = sockfd;
	strncpy(user.name,msg,NAMES_SIZE);
	*(user.name + NAMES_SIZE -1) = '\0';

	list_append(&Users,(void *) &user);

	// Agregar a la sala default #!

	while (1) {

		// Leer un mensaje del cliente
		readBytes(sockfd, (void *) &header,sizeof(header));
		readBytes(sockfd, (void *) msg, header.msgSize);
		cleanString(msg);

		switch (header.code) {

		case MSG_CODE_SHUT:
			break; // #! NOT DONE

		case MSG_CODE_SAL:
			sal(sockfd);
			break;
		case MSG_CODE_USU:
			usu(sockfd);
			break;
		case MSG_CODE_MEN:
			men(sockfd,msg);
			break;
		case MSG_CODE_SUS:
			sus(sockfd,msg);
			break;
		case MSG_CODE_DES:
			des(sockfd);
			break;
		case MSG_CODE_CRE:
			cre(sockfd,msg);
			break;
		case MSG_CODE_ELI:
			// eli() #!
			break;
		case MSG_CODE_FUE:
			fue(sockfd);
			free(msg);
			pthread_exit(NULL);
			break;
		}

		*msg = '\0';
	}
	return NULL;
}

/*
 * Main. NO COMPLETO.
 */
int main(int argc, char *argv[]) {
	printf("Inicio");
	int sockfd;									// Socket de escucha
	int newsockfd;								// Socket de nueva conexion
	struct sockaddr_in clientAddr, serverAddr;	// Direcciones de Internet
	socklen_t socklen = sizeof(clientAddr);		// Tamano de dir de cliente

	int portNum = DEFAULT_PORT;					// Puerto del servidor
	char *inRoomName;
	Room initialRoom;

	pthread_t pthreadStruct;					// Contenedor Dummy
	threadStruct *threadArg;					// Apuntador del arg del hilp
	int status;									// Estatus de crear hilo

	// Setear argumentos
	svOptionHandler(argc,argv,&portNum,&inRoomName);
	printf("Iniciando servidor en %d con %s", portNum, inRoomName);

	// Creacion de socket
	if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		criticalError(SV_SOCKET);
	}

	// Anclaje de socket
	bzero((void *) &serverAddr, sizeof(serverAddr));
	serverAddr.sin_family = AF_INET;
	serverAddr.sin_addr.s_addr = INADDR_ANY;
	serverAddr.sin_port = htons(portNum);

	if (bind(sockfd,(void *)&serverAddr,sizeof(serverAddr)) < 0) {
		criticalError(SV_BIND);
	}

	// Colocar el socket en modo escucha
	if (listen(sockfd, NUM_CONNECTIONS) < 0) {
		criticalError(SV_LISTEN);
	}

	// Inicializar las listas de salas y usuarios (con la sala inicial)
	list_new(&Rooms,sizeof(Room));
	list_new(&Users,sizeof(User));

	strncpy(initialRoom.name,inRoomName,NAMES_SIZE);
	*(initialRoom.name + NAMES_SIZE - 1) = '\0';
	list_new(&(initialRoom.users),sizeof(User *));
	list_append(&Rooms,&initialRoom);

	while (1) {

		// Aceptar una conexion
		if ((newsockfd = accept(sockfd,(struct sockaddr *) &clientAddr,
				&socklen)) < 0) {
			criticalError(SV_ACCEPT);
		}

		// Crear el thread que maneje la conexion con el cliente en especifico
		threadArg = malloc(sizeof(threadStruct)); // El thread libera
		threadArg->sockfd = newsockfd;
		status = pthread_create(&pthreadStruct,
								NULL,
								&clientHandler,
								(void *) threadArg);
		if (status != 0) {
			criticalError(69);
		}
	}

	return 0;
}
