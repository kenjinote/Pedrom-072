#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// These are the hunk IDs from the A68K source
#define HunkUnit 999
#define HunkName 1000
#define HunkCode 1001
#define HunkData 1002
#define HunkBSS  1003
#define HunkR32  1004
#define HunkR16  1005
#define HunkR8   1006
#define HunkExt  1007
#define HunkSym  1008
#define HunkDbg  1009
#define HunkEnd  1010

#define EXTRA_INFO(x)
#define ERROR(x) {printf(x); exit(1); }
#define ERROR1(x,y) {printf(x,y); exit(1); }
#define ERROR2(x,y,z) {printf(x,y,z); exit(1); }

void removeExt(const char *source, char *dest)
{ int i = 0; while (source[i]!=0 & source[i]!='.') dest[i++] = source[i]; dest[i] = 0; }

long ReadDWord(FILE *fp)
{ long v=fgetc(fp)<<24; v|=fgetc(fp)<<16; v|=fgetc(fp)<<8; v|=fgetc(fp); return v; }

void WriteDWord(FILE *fp, long x)
{ fputc(x>>24, fp); fputc((x>>16)&0xFF, fp); fputc((x>>8)&0xFF, fp); fputc(x&0xFF, fp);}

void WriteWord(FILE *fp, long x)
{ fputc(x>>8&0xFF, fp); fputc(x&0xFF, fp);}

long	ReadAmigaObj(FILE *fp)
{
   // Process each hunk in the object file
   while (!feof(fp))
   	{
	int hunk=ReadDWord(fp);
	EXTRA_INFO(printf("Hunk: %d\n", hunk));
	if (hunk==HunkCode)
		{
		int size=ReadDWord(fp); // Size is in DWORDs
		return size*4;
		}
	else if (!feof(fp))
		{
		int size=ReadDWord(fp); // Size is in DWORDs
		// Unused hunk, so skip it
	        EXTRA_INFO(printf(">Unused section\n"));
		fseek(fp,size<<2,SEEK_CUR);
		}
	}

   return 0;
}

void	main(int argc, char *argv[])
{
	FILE *fTib, *fObj, *fOut;
   	int size, count, i;
   	short c;
   	
	if (argc != 4)
		ERROR("ERROR: mergetib tibname.tib obj.o output.tib")
	// Read Obj
	fObj=fopen(argv[2],"rb");
	if (fObj == NULL)
		ERROR1("Can not open object file: %s",argv[2]);
	size = ReadAmigaObj(fObj);
	if (size == 0)
		ERROR("No code to merge in object file");
	// Read TIB
	fTib = fopen(argv[1],"rb");
	if (fTib == NULL)
		ERROR1("Can not open TIB file: %s", argv[1]);
	// Write TIB
	fOut = fopen(argv[3],"wb");
	if (fOut == NULL)
		ERROR1("Can not open Output file: %s", argv[3]);
	// Header
	count = 0;
	while (count != 4)
		{
		c = fgetc(fTib);
		if (c == 0xCC)
			count++;
		else	count = 0;
		fputc(c, fOut);
		}
	// Main
	for(i = 0 ; i < size ; i++)
		{
		c = fgetc(fTib);	// Read TIB
		c = fgetc(fObj);	// Read OBJ
		fputc(c, fOut);		// Write Out
		}
	while (!feof(fTib))
		{
		c = fgetc(fTib);	// Read TIB
		fputc(c, fOut);		// Write Out
		}
	fclose(fOut);
	fclose(fTib);
	fclose(fObj);
}
