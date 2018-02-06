/********************************************************************************

  con6502

  Command to execute 6502 programs on standard console.


  6502 emulation Copyright (C) Marat Fayzullin 1996-2007 
                               Alex Krasivsky  1996                   
********************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../common/common.h"
#include "m6502.h"

#define STDERR stderr

// We store output printed to screen in cyclic buffer.
// When we print using assert instruction ($df), con6502 performs test, that output printed to screen
// equals byte for byte to the output specified in assert.
// If not, error is reported and execution ends.

#define OUT_BUF_SIZE 4096
UInt8 out_buf[OUT_BUF_SIZE];
UInt16 out_buf_first = 0;
UInt16 out_buf_last = 0;

Bool   StrEqual(char * str1, char * str2)
{
	if (str1 == str2) return true;
	if (str1 == NULL || str2 == NULL) return false;

#ifdef __Linux__
	return strcasecmp(str1, str2) == 0;
#else
	return _stricmp(str1, str2) == 0;
#endif
}

M6502 cpu;
byte mem[0xffff];
word load_adr;
Bool done;
UInt16  result;

void Wr6502(register word Addr,register byte Value)
{
	mem[Addr] = Value;
}

byte Rd6502(register word Addr)
{
	return mem[Addr];
}

byte Loop6502(register M6502 *R)
{
	if (done) return INT_QUIT;
	return INT_NONE;
}

byte Patch6502(register byte Op,register M6502 *R)
{
	if (Op == 0xff) {
		out_buf[out_buf_last++] = R->A;
		if (out_buf_last == OUT_BUF_SIZE) out_buf_last = 0;
		printf("%c", R->A);
		return 1;
	} else if (Op == 0xdf) {
		if (out_buf[out_buf_first] != R->A) {
			printf("*** Unexpected output\n");
			done = true;
			result = 3;
		}
		out_buf_first++;
		if (out_buf_first == OUT_BUF_SIZE) out_buf_first = 0;
		return 1;

	// Normal application exit
	} else if (Op == 0xef) {
		done = true;
		return 1;

	// Assert failure
	} else if (Op == 0xcf) {
		done = true;
		result = 3;
		return 1;
	} else {
		return 0;
	}
}

Bool ParseAdr(char * Txt, word * pA)
{
	word a;
	char c;
	char * t = Txt;
	a = 0;
	if (*t == '$') {
		t++;
		do {
			c = *t;
			if (c>='0' && c<='9') {
				c = c - '0';
			} else if (c >= 'a' && c <='f') {
				c = c - 'a' + 10;
			} else if (c >= 'A' && c <='F') {
				c = c - 'A' + 10;
			} else {
				return false;
			}
			a = a * 16 + c;
		} while(true);
		
	} else {
		a = atoi(Txt);
	}
	*pA = a;
	return true;
}

word LoadFile(char * Filename, word load_adr)
{
	int b1, b2;
	word adr;
	FILE * f;
	word size;
	char filename2[1024];

	adr = 0xffff;
	f = fopen(Filename, "rb");
	if (f == NULL) {
		sprintf(filename2, "%s.c65", Filename);
		f = fopen(filename2, "rb");
		if (f == NULL) {
			fprintf(STDERR, "Failed to load file %s\n", Filename);
			exit(2);
		}
	}


	b1 = fgetc(f);
	b2 = fgetc(f);

	if (b1 != -1 && b2 != -1) {
		adr = b2 * 256 + b1;
		size = fread(&mem[adr], 1, 0xffff - adr, f);
	} else {
		fprintf(STDERR, "Illegal file format\n");
		exit(2);
	}
	fclose(f);

	return adr;
}

int main(int argc, char *argv[])
{
	UInt8 i;
	// filename -a load_addr

	result = 0;

	load_adr = 512;		// 0 page for variables, 1 page stack
	i = 1;
	while (i < argc) {		
		if (StrEqual(argv[i], "-A")) {
			i++;
			if (!ParseAdr(argv[i], &load_adr)) {
			    fprintf(STDERR, "Invalid format of load address");
				exit(-1);
			}
		} else {
			break;
		}
		i++;
	}

    if (i == argc) {
        fprintf(STDERR, "Usage:\n"
		"%s [options] file\n"
		"  -a Load address of binary file\n"
		, argv[0]);
        exit(1);
    }

	load_adr = LoadFile(argv[i], load_adr);

	Reset6502(&cpu);
	cpu.PC.W = load_adr;
	done = false;
	Run6502(&cpu);

	return result;
}
