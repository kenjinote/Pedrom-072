#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

typedef struct {
unsigned char *code;
unsigned long size;
enum {TI92PLUS, TI89, V200} output;
} obj;

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
// Symbol entry type IDs
#define TypeEnd 0
#define TypeExport 1
#define TypeImport32 0x81
#define TypeImport16 0x83
#define TypeImport8 0x84
#define TypeImport16Alt 0x8a
#define TypeEqu 0x02

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

void WriteDWordRAM(unsigned char *a, unsigned long x)
{ *a++ = x>>24; *a++ = (x>>16)&0xFF; *a++ = (x>>8)&0xFF; *a++ = x&0xFF;}

void WriteWord(FILE *fp, long x)
{ fputc(x>>8&0xFF, fp); fputc(x&0xFF, fp);}

obj	ReadAmigaObj(const char *filename)
{
   obj o = {NULL, 0, 0};
   unsigned int size, type, i, ofs;
   char sym[100];
   FILE *fp = fopen(filename, "rb");
   if (fp == NULL) return o;
   
   // Process each hunk in the object file
   while (!feof(fp))
   	{
	int hunk=ReadDWord(fp);
	if (hunk==HunkCode)		// Code
		{
		if (o.code) ERROR("Duplicate CODE section");
		o.size = ReadDWord(fp) * 4; // Size is in DWORDs
		o.code = malloc(o.size);
		if (o.code == NULL) ERROR("Memory overflow");
		fread(o.code, o.size, 1, fp);
		}
	else if (hunk==HunkExt) 	// Exports references
		while (1)
			{
			size=ReadDWord(fp);
			// High byte of size contains type byte
			type=(size>>24)&0xff;
			size&=0xffffff;
			if (type==TypeEnd)
				break;
			// Read name of symbol
			for (i=0;i<(size<<2);i++)
				sym[i]=fgetc(fp);
			sym[i]=0;
			if (type==TypeExport)
				{
				// Get offset of exported function
				ofs=ReadDWord(fp);
				// Check for special export names
				if (!strcmp(sym,"_ti89"))
					o.output = TI89;
				else if (!strcmp(sym,"_ti92plus"))
					o.output = TI92PLUS;
				else if (!strcmp(sym,"_v200"))
					o.output = V200;
				}
			else	ERROR2("Unsupported Type: %d for symbol: %s", type, sym);
			}
	else if (hunk==HunkR32)
		{
		// Read each section's relocations
		while (1)
			{
			int count=ReadDWord(fp);
			if (!count) break;
			ReadDWord(fp);
            for(i = 0 ; i < count; i++)
					ReadDWord(fp);
			}
		}
	else if (hunk==HunkR16 || hunk==HunkR8)
		{ERROR("Error: 16-bit & 8-bit relocations are unsupported\n");}
	else if (hunk==HunkEnd) // End of code or BSS section
		{
			break;
		}
	else 	{
		size=ReadDWord(fp); // Size is in DWORDs
		fseek(fp,size*4,SEEK_CUR);
		}
	}

   fclose(fp);
   return o;
}

char	TIBHeader[] = {
	/*0x2a,0x2a,0x54,0x49,0x46,0x4c,0x2a,0x2a,0x02,0x00,
	0x00,0x00,0x29,0x07,0x20,0x02,0x08,0x62,0x61,0x73,
	0x65,0x63,0x6f,0x64,0x65,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x88,0x23,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
	0x00,0x00,0x00,0x01,0xb3,0xd9,0x12,0x00,*/0x80,0x0f,
	0x00,0x12,0xd9,0x6a,0x80,0x11,0x01,0x80,0x21,0x0b,
	0x80,0x32,0x00,0x52,0x80,0xa1,0x01,0x80,0x4d,0x1d,
	0x41,0x64,0x76,0x61,0x6e,0x63,0x65,0x64,0x20,0x4d,
	0x61,0x74,0x68,0x65,0x6d,0x61,0x74,0x69,0x63,0x73,
	0x20,0x53,0x6f,0x66,0x74,0x77,0x61,0x72,0x65,0x03,
	0x26,0x09,0x04,0x0a,0x7c,0x24,0x15,0x02,0x0d,0x40,
	0xf2,0xac,0xe4,0x00,0x95,0x7e,0x1a,0xaa,0x3c,0x57,
	0x88,0x43,0x27,0x35,0xd2,0x6b,0xd5,0x46,0xfa,0x1e,
	0x10,0x6d,0xd7,0x0d,0xb1,0x6d,0x72,0x09,0x79,0x12,
	0xf2,0x1c,0xdd,0x78,0x1e,0x8f,0x8e,0x70,0x46,0x75,
	0x09,0x38,0x14,0xd4,0x87,0x20,0xa1,0x41,0x8e,0xe9,
	0x14,0x7e,0x1f,0x63,0xc5,0xd9,0x88,0xbf,0x22,0xa4,
	0x2f,0xbd,0xe9,0x48,0x80,0x7f,0x00,0x12,0xd8,0xec,
	0xcc,0xcc,0xcc,0xcc};

char	TIBEnd[] = {
	0x2a,0x1a,0xdb,0x40,0x02,0x0d,0x40,0xe4,0x84,0x07,
	0x17,0xac,0x6e,0xee,0xd2,0x8e,0x71,0x3b,0x29,0x23,
	0x31,0x1e,0x1e,0xf3,0x0f,0x4c,0x92,0xa7,0xf1,0x29,
	0xf0,0x9c,0xbc,0x5d,0x05,0x6f,0x19,0x2c,0xc5,0x7d,
	0x70,0x81,0x2c,0x78,0xb3,0x64,0x5b,0x61,0x6e,0x2a,
	0x81,0x2c,0xd7,0xe0,0xe5,0xd0,0xa8,0xe4,0xc1,0x55,
	0x66,0xc3,0xe9,0xfb,0x20,0x60,0x28,0xd7,0x08,0x35,
	0xa4};

int	WriteTIB(obj o, const char *filename)
{
	FILE *f;
	
	if (o.code == NULL) return -1;
	
	if (o.output == TI89)		TIBHeader[0x56] = 3;
	else if (o.output == V200)	TIBHeader[0x56] = 8;
	
	WriteDWordRAM(&TIBHeader[0x50], o.size + 126+8);
	WriteDWordRAM(&TIBHeader[0xCE], o.size + 8);
	
	f = fopen(filename, "wb");
	fwrite(TIBHeader, sizeof(TIBHeader), 1, f);
	fwrite(o.code, o.size, 1, f);
	fwrite(TIBEnd, sizeof(TIBEnd), 1, f);
	fclose(f);

    return 0;
}

void	main(int argc, char *argv[])
{
	obj o;
	char	Name[100];
	
	if (argc != 2)
		ERROR("ERROR: maketib obj.o")
	o = ReadAmigaObj(argv[1]);
	if (o.code == NULL)
		ERROR1("ERROR: Bad object file %s", argv[1]);
	removeExt(argv[1], Name);
	strcat(Name, ".tib");
	WriteTIB(o, Name);
}
