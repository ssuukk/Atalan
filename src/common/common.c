/*

Common utilities

Memory management, strings etc.

(c) 2010 Rudolf Kudla 
Licensed under the MIT license: http://www.opensource.org/licenses/mit-license.php

*/


#include "common.h"
#pragma warning (disable: 4996)

#if defined(__Windows__)
    #include <windows.h>
#endif

#if defined(__Linux__)
    #include <stdarg.h>
#endif

extern void * MemAllocEmpty(long size)
/*
Purpose:
	Allocate memory and set it to zeroes.
*/
{
	void * m = MemAlloc(size);
	memset(m, 0, size);
	return m;
}

char * StrAlloc(char * str)
{
	if (str != NULL) str = strdup(str);
	return str;
}

char * StrAllocLen(char * str, UInt16 len)
{
	char * s = (char *)MemAlloc(len + 1);
	memcpy(s, str, len);
	s[len] = 0;
	return s;
}

Bool   StrEqual(char * str1, char * str2)
{
	if (str1 == str2) return true;
	if (str1 == NULL || str2 == NULL) return false;

#ifdef __Linux__
	return strcasecmp(str1, str2) == 0;
#else
	return stricmp(str1, str2) == 0;
#endif
}
Bool   StrEqualPrefix(char * str1, char * str2, int num)
{
	if (str1 == str2) return true;
	if (str1 == NULL || str2 == NULL) return false;

#ifdef __Linux__
	return strncasecmp(str1, str2, num) == 0;
#else
	return strnicmp(str1, str2, num) == 0;
#endif
}

char * PathFilename(char * path)
/*
Purpose:
	Find filename in path.
	Filename starts after last dir separator in the filename.
*/
{
	char * p = path;
	char * last_slash = NULL;
	while(*p != 0) {
		if (*p == '/' || *p == '\\') last_slash = p;
		p++;
	}
	return (last_slash == NULL)?path:last_slash+1;
}

void PathSeparate(char * path, char * dirname, char * filename)
{
	char * pf;
	UInt16 len;
	pf = PathFilename(path);
	len = pf - path;

	// Copy dir to other place
	if (dirname != NULL && path != dirname) {		
		memcpy(dirname, path, len);
		dirname[len] = 0;
	}

	// Copy file path
	if (filename != NULL) {
		strcpy(filename, pf);
	}

	// If path and dirname are same, just mark the end of dir in the path
	if (path == dirname) {
		path[len] = 0;
	}
}

void PathParent(char * path)
{
	char * s;
	UInt16 len;
	len = StrLen(path);
	if (len < 2) return;
	s = path + len - 1;
	s--;		// skip last dir separator
	while(s > path) {
		if (*s == DIRSEP) { 
			*(s+1) = 0;
			break;
		}
		s--;
	}
}

void PathCutExtension(char * path, char * ext)
{
	UInt16 path_len;
	UInt16 ext_len;

	path_len = StrLen(path);
	ext_len = StrLen(ext);

	if (path_len > ext_len+1) {
		path_len -= ext_len + 1;
		if (path[path_len] == '.' && StrEqual(ext, &path[path_len+1])) {		
			path[path_len] = 0;
		}
	}
}

void PathMerge(char * path, char * dirname, char * filename)
{
	UInt16 len;
	strcpy(path, dirname);
	len = strlen(path);
	strcpy(path+len, filename);
}

void GetApplicationDir(char * name, char * path)
{
#ifdef __Windows__
	GetModuleFileName(NULL, path, MAX_PATH_LEN);
	PathSeparate(path, path, NULL);
#else
	PathSeparate(name, path, NULL);
#endif
}

/****************************************/
/*Implementation of Levenshtein distance*/
/****************************************/

int minimum(int a,int b,int c)
/*Gets the minimum of three values*/
{
  int min=a;
  if(b<min)
    min=b;
  if(c<min)
    min=c;
  return min;
}

Int16 StrEditDistance(char * s, char * t)
/*Compute levenshtein distance between s and t*/
{
  //Step 1
  int k,i,j,n,m,cost,*d,distance;
  n = StrLen(s); 
  m = StrLen(t);

  if (n!=0&&m!=0) {
    d = (int *)MemAlloc((sizeof(int))*(m+1)*(n+1));
    m++;
    n++;
    //Step 2	
    for(k=0;k<n;k++) d[k]=k;
    for(k=0;k<m;k++) d[k*n]=k;
    //Step 3 and 4	
    for (i=1;i<n;i++) {
      for (j=1;j<m;j++) {
        //Step 5
        if (s[i-1]==t[j-1])
          cost=0;
        else
          cost=1;
        //Step 6			 
        d[j*n+i]=minimum(d[(j-1)*n+i]+1,d[j*n+i-1]+1,d[(j-1)*n+i-1]+cost);
      }
	}
    distance=d[n*m-1];
    free(d);
    return distance;
  }
  else 
    return -1; //a negative return value means that one or both strings are empty.
}


#if defined(__Windows__)
    #include <windows.h>
#endif

UInt16 G_OLD_CP;
UInt8 G_COLOR;
FILE * G_PRINT_OUTPUT;		// either STDOUT or STDERR
FILE * G_PRINT_LOG;			// file into which the copy of output gets printed (possibly in HTML)
Bool   G_IN_COLOR;			// if in color

void PrintInit()
{
#ifdef __Windows__
	G_OLD_CP = GetConsoleOutputCP();
	SetConsoleOutputCP(CP_UTF8);
#endif
	PrintDestination(stdout);
	PrintColor(RED+GREEN+BLUE);

	G_IN_COLOR = false;
	G_PRINT_LOG = NULL;
}

void PrintCleanup()
{
#ifdef __Windows__
	SetConsoleOutputCP(G_OLD_CP);
#endif
}

void PrintLog(FILE * file)
{
	if (G_PRINT_LOG != NULL) {
		fprintf(G_PRINT_LOG, "\n</pre></body></html>\n");
	}
	G_PRINT_LOG = file;
	if (G_PRINT_LOG != NULL) {
		fprintf(G_PRINT_LOG, "<html><body bgcolor=\"black\"><pre>\n");
	}
}

FILE * PrintDestination(FILE * file)
{
	FILE * f = G_PRINT_OUTPUT;
	G_PRINT_OUTPUT = file;
	return f;
}

UInt8 PrintColor(UInt8 color)
/*
Purpose:
	Change the color of printed text.
*/
{
	UInt8 old_color = G_COLOR;
	UInt8 r, g, b, h;
#ifdef __Windows__
	HANDLE hStdout; 
	hStdout = GetStdHandle(STD_OUTPUT_HANDLE); 
	SetConsoleTextAttribute(hStdout, color);
#endif
	G_COLOR = color;

	if (G_PRINT_LOG != NULL) {
		if (G_IN_COLOR) { fprintf(G_PRINT_LOG, "</font>"); }
		h = 200; if (FlagOn(color, LIGHT)) h = 255;
		r = (FlagOn(color, RED))?h:0;
		g = (FlagOn(color, GREEN))?h:0;
		b = (FlagOn(color, BLUE))?h:0;

		fprintf(G_PRINT_LOG, "<font color=\"#%2x%2x%2x\">", r, g, b);
	}
	return old_color;
}

void PrintHeader(UInt8 level, char * text, ...)
/*
Purpose:
	Print header to output.
*/
{
	char buffer[256];
	UInt16 len, half_len;
	UInt8 color;
	va_list argp;
	char * hchr;

	if (text == NULL) text = "";

	va_start(argp, text);
	vsprintf(buffer, text, argp);
	va_end(argp);

	len = StrLen(buffer);
	if (len > 70) {
		len = 2;
	} else {
		len = 70 - len;
	}

	hchr = "=";
	if (level > 1) hchr = "-";
	if (level > 2) hchr = ".";

	if (G_PRINT_LOG != NULL) {
		fprintf(G_PRINT_LOG, "<h%d>", level);
	}

	color = PrintColor(RED+GREEN);
	half_len = len / 2;
	PrintRepeat(hchr, half_len);
	Print(" ");
	Print(buffer);
	Print(" ");
	PrintRepeat(hchr, len - half_len);
	PrintColor(color);
	if (G_PRINT_LOG != NULL) {
		fprintf(G_PRINT_LOG, "</h%d>", level);
	}
	Print("\n");
}


void Print(char * text)
{
	if (text != NULL) {
		fprintf(G_PRINT_OUTPUT, "%s", text);
	}
	if (G_PRINT_LOG != NULL) {
		fprintf(G_PRINT_LOG, "%s", text);
	}
}

void PrintChar(char c)
{
	fputc(c, G_PRINT_OUTPUT);
	if (G_PRINT_LOG != NULL) {
		fputc(c, G_PRINT_LOG);
	}
}

void PrintInt(Int32 n)
{
	fprintf(G_PRINT_OUTPUT, "%d", n);
	if (G_PRINT_LOG != NULL) {
		fprintf(G_PRINT_LOG, "%d", n);
	}
}

void PrintEOL()
{
	Print("\n");
}


void PrintRepeat(char * text, UInt16 cnt)
{
	while(cnt-- > 0) {
		Print(text);
	}
}

void PrintFmt(char * text, ...)
{
	char buffer[256];
	va_list argp;
	va_start(argp, text);
	vsprintf(buffer, text, argp);
	va_end(argp);
	Print(buffer);
}
