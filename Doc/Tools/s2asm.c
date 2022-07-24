#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static	const char	*ReplaceTab[] = {
	"\\0\"", "\",0",
	"\\\"", "\",$22,\"",
	"#0x", "#$",
	"#0X", "#$",
	".ascii", " dc.b",
	"%d0", "d0",
	"%d1", "d1",
	"%d2", "d2",
	"%d3", "d3",
	"%d4", "d4",
	"%d5", "d5",
	"%d6", "d6",
	"%d7", "d7",
	"%a0", "a0",
	"%a1", "a1",
	"%a2", "a2",
	"%a3", "a3",
	"%a4", "a4",
	"%a5", "a5",
	"%a6", "a6",
	"%a7", "a7",
	"%sp", "sp",
	"%pc", "pc",
	".byte", " dc.b",
	".long", " dc.l",
	".word", " dc.w",
	".even", " EVEN",
	".globl", " xdef",
	".bss", "", //" BSS",
	".data", "", //" DATA",
	".xdef", " xdef",
	".text", "",
	".dc", " dc",
	".ds", " ds",
	"movm", " movem",
	"__00", "@00",
	".comm", " ds.b",
	"/*", ";",
	"//", ";",
	"jbeq", " beq",
	"jbne", " bne",
	"jbhi", " bhi",
	"jbls", " bls",
	"jbge", " bge",
	"jbgt", " bgt",
	"jble", " ble",
	"jblt", " blt",
	"jbcc", " bcc",
	"jbcs", " bcs",
	"jbmi", " bmi",
	"jbpl", " bpl",
	"jbra", " bra",
	"jbsr", " jsr",
	"pc@(2,d0:w", "2(pc,d0.w",
	".l,",",",
	".w,",",",
	"mov."," move.",
	"#APP","",
	"#NO_APP","",
	", ",",",
	"+2.l","+2"
	 };

// Replace only the first substring
char	*strreplace(const char *source, const char *find, const char *replace, char *dest)
{
	char	*a = strstr(source, find);
	if (a == NULL)
		strcpy(dest, source);
	else	{
		while (source < a)
			*dest++ = *source++;
		source += strlen(find);
		while (*replace)
			*dest++ = *replace++;
		while (*source)
			*dest++ = *source++;			
		*dest = 0;
		}
	return a;
}


int	FGetLine(FILE *F, char* buffer)
{
	int c;

	c = fgetc(F);
	while ( (c != EOF) && (c != '\n'))
		{
		*(buffer++) = c;
		c = fgetc(F);
		}
	*(buffer++) = '\0';
	return (!(c == EOF));
}
/*
static	char	LabelTab[2000*128];
int	LabelCur = 0;

void	AddNewLabel(const char *label)
{
	char *a=&LabelTab[LabelCur*128];
	
	while (*label != 0 && *label != ':')
		*a++ = *label++;
	*a = 0;
	LabelCur++;
}

int	IsLabel(const char *str)
{
	int i;
	const char *a = LabelTab;

	for(i = 0 ; i < LabelCur ; i++, a += 128)
		if (strcmp(str, a) == 0)
			return 1;
	return 0;
}
*/

int	ReplaceOp(char *buffer)
{	
	// Search for a number in op1
	char *a0 = strpbrk(buffer, "0123456789"), *a1 = a0, *a2;
	int len;
	
	// Warning d0-d7/a0-a7
	if (a0 != NULL)
		{
		// (number, -> number(
		if (a0[-1] == '(')
			{
			while (isdigit(*a0))
				a0[-1] = *a0++;
			a0[-1] = '(';
			memmove(a0, a0+1, strlen(a0)+1-1);
			return 1;
			}
		else if (a0[-1] == '-' && a0[-2] == '(')
			{
			a0[-2] = '-';
			while (isdigit(*a0))
				a0[-1] = *a0++;
			a0[-1] = '(';
			memmove(a0, a0+1, strlen(a0)+1-1);
			return 1;
			}			
		else if (a0[-1] != '#' && a0[-1] != 'd' && a0[-1] != 'a')
			{
			// number.w -> (number).w	
			while (isdigit(*a1))
				a1++;
			if (*a1 == '.')
				{
				len = a1 - a0;
				memmove(a0+2, a0, strlen(a0)+1);
				*a0++ = '(';
				while (len--)
					*a0++ = a0[1];
				*a0 = ')';
				return 1; 
				}
			else if (*a1 == ':' && (a1[-1] == 'd' || a1[-1] == 'a'))
				{
				*a1  = '.';
				return 1;
				}
			}
		}
	// (xxx,yyy) -> 0(xxx,yyy) xxx not a number
	if (	(a0=strchr(buffer, '(')) != NULL && 
		!isdigit(a0[-1]) &&
		!isalpha(a0[-1]) && 
		(a1=strchr(a0, ',')) != NULL && 
		(a2=strchr(a0, ')')) != NULL &&
		a2 > a1) 
			{
			memmove(a0+1, a0, strlen(a0)+1);
			*a0 = '0';
			return 1;
			}
	// NananaLabel-NananaLabel.b(
	if (a0 != NULL && a0[-1] == 'b' && a0[-2] == '.')
		{
		for( a1 = a0 ; *a1 != '-' && a1 > buffer ; a1--);
		if (*a1 == '-')
			{
			memmove(a1, a0, strlen(a0)+1);
			return 1;
			}
		}
		
	return 0;
}

int main(int argc, char *argv[])
{
 FILE *F1,*F2;

 char	buffer1[25600];
 char	buffer2[25600];
 int i,j,k, skipline = 10;
 char* a;

 if (argc < 5)
	{
	printf("Convert S file to Asm file (c) 2003 PpHd\n");
	printf("Usage: cvts2asm file.s file.asm LabelName SkipLine\n");
	exit(1);
	}
 F1 = fopen(argv[1],"r");
 F2 = fopen(argv[2],"w");

 if ((F1 == NULL) || (F2 == NULL))
	{
	printf("Can't open files %s and %s!\n", argv[1], argv[2]);
	exit(1);
	}
 skipline = atoi(argv[4]);
 
 while (skipline-- && FGetLine(F1,buffer1));
 
 while (FGetLine(F1,buffer1))
	{
	for(i = 0 ; i < sizeof(ReplaceTab)/sizeof(ReplaceTab[0]) ; i+=2 )
		{
		do {
			a = strreplace(buffer1, ReplaceTab[i], ReplaceTab[i+1], buffer2);
			if (a) strcpy(buffer1, buffer2);
		} while(a);
		}
	do {
		a = strreplace(buffer1, ".L", argv[3], buffer2);
		if (a) strcpy(buffer1, buffer2);
	} while(a);

	ReplaceOp(buffer1);
	// jxxx yyy -> if y = '(' => jsr else bxxx
	fprintf(F2,"%s\n",buffer1);
	}
 fclose(F1), fclose(F2);
 exit(0);
}
