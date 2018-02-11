export LC_ALL = C

PRG = atalan
OBJ = *.o
CONFIGURATION = 

SYSTEM_NAME := $(shell uname -s)
SYSTEM_ARCH := $(shell uname -m)


CHARSET = UTF-8

prefix = $(DESTDIR)/usr/local
BINDIR = $(DESTDIR)/usr/local/bin
LIBDIR = $(DESTDIR)/usr/local/lib
MANDIR = $(DESTDIR)/usr/local/share/man

SOURCES= gen.c translate.c emit.c errors.c instr.c lexer.c main.c mem_heap.c \
 opt_blocks.c opt_live.c opt_values.c opt_var_use.c optimize.c parser.c type.c \
 variables.c type_proc.c opt_loops.c var_set.c bigint.c opt_global.c var_int.c \
 parse_type.c opt_loop_shift.c

OBJS= ../common/common.o \
         gen.o translate.o emit.o errors.o instr.o lexer.o main.o mem_heap.o \
 opt_blocks.o opt_live.o opt_values.o opt_var_use.o optimize.o parser.o type.o \
 variables.o type_proc.o opt_loops.o var_set.o bigint.o opt_global.o var_int.o \
 parse_type.o opt_loop_shift.o

CC = gcc
CXX = gcc

CFLAGS = -g -O0 -Wall -D__$(SYSTEM_NAME)__ -D__$(SYSTEM_ARCH)__
###### generic rules #######
all:	atalan
	@echo System name: $(SYSTEM_NAME)
	@echo System Arch: $(SYSTEM_ARCH)

atalan: $(OBJS)

	$(CC) $(CFLAGS) -o $@ $(OBJS) -lm && echo "Compile OK"

%.o: %.c

	@$(CC) -c $(CFLAGS) $<

%.asm: %.atl
	./atalan -a -I src examples/$<

%.xex: %.asm
	mads examples/$< -i:./src -o:examples/$@ -x

clean:
	rm -f atalan $(OBJS)

