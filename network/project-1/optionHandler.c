/*
 * optionHandler.c
 *
 * Handles the optional/mandatory arguments for the dice game.
 *
 * Created on: May 22, 2013
 *   Last Mod: Jun 27, 2013
 *	Authors:
 *			 Manuel Alejandro Pacheco - 10-10524 - manuelalejandropm@gmail.com
 *  		   Cristian Adrian Medina - 10-10445 - cmedina270793@gmail.com
 */

#include "optionHandler.h"

void clOptionHandler(int argc, char *argv[],
					char **hA,
					char **pN,
					char **uN,
					char **fN) {

	char ch;							// Temporal char

	// Option handling
	while ((ch = getopt(argc,argv,"h:p:n:")) > 0) {
		switch (ch) {

		case 'h':
			*hA = optarg;
			break;

		case 'p':
			*pN = optarg;
			break;

		case 'n':
			*uN = optarg;
			break;

//		case 'a':
//			*fN = optarg;
//			break;

		// Error (argument error) Case
		default:
			criticalError(CL_ARGS);
			break;
		}
	}
}

void svOptionHandler(int argc, char *argv[],
					int *pN,
					char **rN) {
	int temp;						   // Temporal integer
	char ch;							// Temporal char
	char* strtolp;					  // Pointer used in strtol (integer conv)

	// Initialization of variables
	errno = 0;

	// Option handling
	while ((ch = getopt(argc,argv,"p:s:")) > 0) {
		switch (ch) {

		case 'p':
			temp = (int) strtol(optarg,&strtolp,10);
			if (temp > 0 && !errno && optarg != strtolp && *strtolp == '\0')
				*pN = temp;
			else
				criticalError(SV_ARGS);
			break;

		case 's':
			*rN = optarg;
			break;

		// Error (argument error) Case
		default:
			criticalError(SV_ARGS);
			break;
		}
	}
}


