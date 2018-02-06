export LC_ALL = C

CONFIGURATION = 

SYSTEM_NAME := $(shell uname -s)
SYSTEM_ARCH := $(shell uname -m)


CHARSET = UTF-8

prefix = $(DESTDIR)/usr/local
BINDIR = $(DESTDIR)/usr/local/bin
LIBDIR = $(DESTDIR)/usr/local/lib
MANDIR = $(DESTDIR)/usr/local/share/man

SOURCES= common.c
OBJS= common.o

CC = gcc
CXX = gcc

CFLAGS = -g -Wall -D__$(SYSTEM_NAME)__ -D__$(SYSTEM_ARCH)__
###### generic rules #######
all: $(OBJS)

	#$(CC) $(CFLAGS) $(OBJS) && echo "Compile OK"

%.o: %.c

	@$(CC) -c $(CFLAGS) $<

clean:
	rm -f $(OBJS)

