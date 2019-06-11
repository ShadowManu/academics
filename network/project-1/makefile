TARGET = listTester schat cchat 
CC = gcc
HEADERS = list.h criticalErrors.h dataHelper.h optionHandler.h

all: $(TARGET)

debug: CFLAGS += -g -O0 -Wall -Wextra
debug: $(TARGET)

listTester: listTester.o list.o criticalErrors.o
	$(CC) $(CFLAGS) -o $@ $^

schat: schat.o dataHelper.o list.o criticalErrors.o optionHandler.o
	$(CC) $(CFLAGS) -pthread -o $@ $^

cchat: cchat.o dataHelper.o list.o criticalErrors.o optionHandler.o
	$(CC) $(CFLAGS) -pthread -o $@ $^


%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -o $@ -c $< 

clean:
	rm *.o
	rm $(TARGET)

.PHONY: all debug clean