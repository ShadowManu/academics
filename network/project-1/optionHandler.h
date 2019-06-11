/*
 * optionHandler.h
 *
 * Handles the optional/mandatory arguments for the dice game.
 *
 * Created on: May 22, 2013
 *   Last Mod: Jun 27, 2013
 *    Authors:
 *             Manuel Alejandro Pacheco - 10-10524 - manuelalejandropm@gmail.com
 *  		   Cristian Adrián Medina - 10-10445 - cmedina270793@gmail.com
 */

#include <unistd.h>     // getopt
#include <stdlib.h>     // strtol, exit
#include <errno.h>      // strtol
#include <string.h>     // strerror
#include <stdio.h>      // puts, printf

#ifndef OPTIONHANDLER_H_
#define OPTIONHANDLER_H_

#include "criticalErrors.h"

void clOptionHandler(int argc, char *argv[],
					char **hA,
					char **pN,
					char **uN,
					char **fN);

void svOptionHandler(int argc, char *argv[],
					int *pN,
					char **rN);

#endif
