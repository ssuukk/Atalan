/********************************************************************************

  con6502

  Command to execute 6502 programs on standard console.


  Z80 emulation Copyright (C) Marat Fayzullin 1994-2002
                              
********************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../common/common.h"

#define DEBUG

#include "Z80.h"
#include "Debug.c"

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

Z80 cpu;
byte mem[0xffff];
word load_adr;
Bool done;
UInt16  result;

void WrZ80(register word Addr,register byte Value)
{
	mem[Addr] = Value;
}

byte RdZ80(register word Addr)
{
	return mem[Addr];
}

void OutZ80(register word Port,register byte Value)
{
	if (Port == 0) {
		printf("%c", Value);
	}
}

byte InZ80(register word Port)
{
	return 0;
}

word LoopZ80(register Z80 *R)
{
	if (done) return INT_QUIT;
	return INT_NONE;
}

void PatchZ80(register Z80 *R)
{
	done = true;
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
//	int b1, b2;
	word adr;
	FILE * f;
	word size;
	char filename2[1024];

	adr = load_adr;
	f = fopen(Filename, "rb");
	if (f == NULL) {
		sprintf(filename2, "%s.z80", Filename);
		f = fopen(filename2, "rb");
		if (f == NULL) {
			fprintf(STDERR, "Failed to load file %s\n", Filename);
			exit(2);
		}
	}

	size = fread(&mem[adr], 1, 0xffff - adr, f);
	fclose(f);

	return adr;
}

int main(int argc, char *argv[])
{
	UInt8 i;
	// filename -a load_addr

	result = 0;

	load_adr = 0x100;
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
        exit(-1);
    }

	load_adr = LoadFile(argv[i], load_adr);

	ResetZ80(&cpu);
	cpu.PC.W = load_adr;
	done = false;
	RunZ80(&cpu);

	return result;
}
