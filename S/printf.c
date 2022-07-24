/*
;* PedroM - Operating System for Ti-89/Ti-92+/V200.
;* Copyright (C) 2003 PpHd 
;*
;* This program is free software ; you can redistribute it and/or modify it under the
;* terms of the GNU General Public License as published by the Free Software Foundation;
;* either version 2 of the License, or (at your option) any later version. 
;* 
;* This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;* See the GNU General Public License for more details. 
;* 
;* You should have received a copy of the GNU General Public License along with this program;
;* if not, write to the 
;* Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA 
 */

#include "PedroM.h"

#define	ClrScr	ScreenClear

#define _LJUSTIFY         0x1
#define _SIGNED           0x2
#define _BLANKER          0x4
#define _VARIANT          0x8
#define _PRECGIVEN       0x10
#define _LONGSPEC        0x20
#define _SHORTSPEC       0x40
#define _PADZERO         0x80
#define _FPCONV         0x100

typedef struct {
  int flags;
  char *prefix;
  int precision;
  int (*putc)(short, FILE *);
  char *hextab;
  int width;
  float f;
} vcbprintf_display_rec;

#define intofdigit(x) ((x)-'0')
#define xputc(dr, ch, f) (dr->putc(ch, f))

#define pr_padding(dr, ch, n, f)  while (--n>=0) charcount++, xputc(dr, ch, f);

#define pre_padding(dr, f)                                                \
        if (!(flags&_LJUSTIFY))                                           \
        {   char padchar = flags & _PADZERO ? '0' : ' ';                  \
            pr_padding(dr, padchar, width, f); }

#define post_padding(dr, f)                                               \
        if (flags&_LJUSTIFY)                                              \
        {   pr_padding(dr, ' ', width, f); }

inline int vcbprintf_display(FILE *p, int ch, vcbprintf_display_rec *dr, unsigned long v)
{
    int len = 0, charcount = 0, i;
    char buff[32];
    int flags = dr->flags;
    int precision = dr->precision;
    int width = dr->width;
    bcd *b = (bcd *) (&(dr->f));
    short expo;
    char sign, bzero;
    
    if (!(dr->flags & (_FPCONV+_PRECGIVEN)))
    	dr->precision = 1;
    switch (ch)
    {
    case 'p':
    case 'X':
    case 'x':   while(v != 0)
                    {
                    buff[len++] = dr->hextab[v & 0xf];
                    v = v >> 4;
                    }
                break;
    case 'o':   while(v != 0)
                    {
		     buff[len++] = '0' + (v & 7);
                     v = v >> 3;
                    }
                if (flags & _VARIANT)
                	dr->precision--;
                break;
    case 'b':   while(v != 0)
                    {
		     buff[len++] = '0' + (v & 1);
                     v = v >> 1;
                    }
                break;
    case 'u':
    case 'i':
    case 'd':	while(v != 0)
                    {
		    buff[len++] = '0' + (char)(v % 10);
                    v = v / 10;
                    }
                break;
    default:    // Float
    		// Exponent
    		expo = (b->exponent & 0x7FFF) - 0x4000;
    		if (expo == (0x7FFF- 0x4000))
    			{
    			buff[len++] = 'f';
    			buff[len++] = 'e';
    			buff[len++] = 'd';
    			buff[len++] = 'n';
    			buff[len++] = 'u';
    			}
    		else if (expo == (0x2000-0x4000))
			{
    			buff[len++] = '.';
    			buff[len++] = '0';    			
			}
    		else if (expo == (0x6000-0x4000))
			{
    			buff[len++] = 190;
    			if (b->exponent & 0x8000)
	    			buff[len++] = '-';
    			else	buff[len++] = '+';
			}    		
    		else	{
	    		sign = 0;
	    		if (expo < 0)
	    			{
	    			expo=-expo;
	    			sign = 1;
	    			}
	    		do  {
			    buff[len++] = '0' + (char)(expo % 10);
	                    expo = expo / 10;
	                    } while (expo != 0) ;
	                if (sign)
	                	buff[len++] ='-';
	                buff[len++]=149;
	    		// Mantisse
	    		bzero = 0;
	    		v = b->mantissa2;
	     		for(i = 0 ; i < 8 ; i++)
	    			{
	    			if (bzero || (v &0x0F))
	    				{
		    			buff[len++] = (v & 0xF) + '0';
	    				bzero = 1;
	    				}
	    			v >>= 4;
	    			}
	    		v = b->mantissa1;
	     		for(i = 0 ; i < 7 ; i++)
	    			{
	    			if (bzero || (v &0x0F))
	    				{
		    			buff[len++] = (v & 0xF) + '0';
	    				bzero = 1;
	    				}
	    			v >>= 4;
	    			}
			if (bzero)
				buff[len++] = '.';
			buff[len++] = (v & 0xF) + '0';
			if (b->exponent & 0x8000)
				buff[len++] = 175;
			}
                break;
    }
    
    if (v==0 && len == 0)
    	buff[len++] = '0';
    	
    if ((precision -= len)<0)
    	precision = 0;
    width -= (precision + len + strlen(dr->prefix));

    if (!(flags & _PADZERO))
        	pre_padding(dr, p);

    {
    	char c;
        char *prefix = dr->prefix;
        for (; (c=*prefix++)!=0; charcount++) xputc(dr, c, p);
    }

    pre_padding(dr, p);
    pr_padding(dr, '0', precision, p);
    charcount += len;
    while (len-- > 0)
    	xputc(dr, buff[len], p);
    post_padding(dr, p);
    return charcount;
}

int vcbprintf (int (*callback)(short,FILE *), FILE *p, const char *fmt, va_list args) 
{
    vcbprintf_display_rec dr_data;
    vcbprintf_display_rec *dr = &dr_data;
    int charcount = 0;
    char ch;
    dr->putc = callback;
    while ((ch = *fmt++) != 0)
    	{
	if (ch != '%')
    		{
    		xputc(dr, ch, p);
    		charcount++;
    		}
        else
        {   int flags = 0, width = 0;
            unsigned long v = 0;
            dr->precision = 0;
            dr->prefix="";
            // Read flags (Ti special flags not supported !)
            for (;;)
	            {   switch (ch = *fmt++)
	                {
		        case '-':   flags = _LJUSTIFY | (flags & ~_PADZERO);
		                    continue;
		        case '+':   flags |= _SIGNED;
	        	            continue;
		        case ' ':   flags |= _BLANKER;
	        	            continue;
		        case '#':   flags |= _VARIANT;
	        	            continue;
		        case '0':   flags |= _PADZERO;
	        	            continue;
			case 'z':
			case '^':
			case '|':	// Ti special Format, not yet supported
				continue;
			default:    break;
	                }
	                break;
	            }
            // Read number field 
            {   long t = 0;
                if (ch=='*')
			{
			t = va_arg(args, int);
			if (t<0)
				{
				t = - t;
				flags ^= _LJUSTIFY;
				}
			ch = *fmt++;
	                }
                else	while (isdigit(ch))
			{
			t = t*10 + intofdigit(ch);
			ch = *fmt++;
			}
                width = t>=0 ? t : 0;
            }
            // Read precision
            if (ch == '.')
		{
		long t = 0;
                ch = *fmt++;
                if (ch=='*')
	                {
			t = va_arg(args, int);
	                ch = *fmt++;
	                }
		else if (ch =='-')	// -1 (Ti specific)
			{
	                ch = *fmt++;
			if (ch == '1')
				{
				t = 6;
				ch = *fmt++;
				}
			else	t = -1;
			}
		else	while (isdigit(ch))
			{
			t = t*10 + intofdigit(ch);
			ch = *fmt++;
                	}
                if (t >= 0)
                	{
                	flags |= _PRECGIVEN;
                	dr->precision = t;
                	}
            	}
            // Read Short or Long ?
            if (ch=='l' || ch=='L')
	            {
		    flags |= _LONGSPEC;
		    ch = *fmt++;
	            }
            else if (ch=='h' || ch=='H')
	            {
	            flags |= _SHORTSPEC;
	            ch = *fmt++;
	            }	// Default Short
	    else    flags |= _SHORTSPEC;

            // Display the var
            switch (ch)
            {
		    case 0:     fmt--;
		                continue;
		    case 's':   {
				char *str = va_arg(args, char *);
				long i, n;
				if (flags & _PRECGIVEN)
					{
					long precision = dr->precision;
					for (n = 0; n < precision && str[n] != 0; n++) continue;
					}
				else	n = strlen(str);
				width -= n;
				pre_padding(dr, p);
				for (i=0; i<n; i++)
					xputc(dr, str[i], p);
				charcount += n;
				post_padding(dr, p);
		                }
		                continue;
		    case 'X':   dr->hextab = "0123456789ABCDEF";
		                dr->prefix = (flags&_VARIANT) ? "0X" : "";
		                goto	DisplayHex;
		    case 'x':   dr->hextab = "0123456789abcdef";
		                dr->prefix = (flags&_VARIANT) ? "0x" : "";
DisplayHex:	                if (flags & _LONGSPEC)
		                	v = va_arg(args, unsigned long);
		                else	v = va_arg(args, unsigned short);
		                if (flags & _PRECGIVEN) flags &= ~_PADZERO;
		                break;
		    case 'b':   dr->prefix = (flags&_VARIANT) ? "0b" : "";
		                if (flags & _LONGSPEC)
		                	v = va_arg(args, unsigned long);
		                else	v = va_arg(args, unsigned short);
		                if (flags & _PRECGIVEN) flags &= ~_PADZERO;
		                break;
		    case 'p':   v = (unsigned long) va_arg(args, void *);
		                dr->hextab = "0123456789abcdef";
		                dr->prefix = (flags&_VARIANT) ? "@" : "";
		                dr->precision = 6;	// Only 24 bits avialble
		                flags |= _PRECGIVEN;
		                break;
		
		    case 'o':   if (flags & _LONGSPEC)
		                	v = va_arg(args, unsigned long);
		                else	v = va_arg(args, unsigned short);
		                dr->prefix = (flags&_VARIANT) ? "0" : "";
		                if (flags & _PRECGIVEN) flags &= ~_PADZERO;
		                break;
		    case 'u':   if (flags & _LONGSPEC)
		                	v = va_arg(args, unsigned long);
		                else	v = va_arg(args, unsigned short);
		                dr->prefix = "";
		                if (flags & _PRECGIVEN) flags &= ~_PADZERO;
		                break;		
		    case 'i':
		    case 'd':   {
				long w;
				if (flags & _LONGSPEC)
			        	w = va_arg(args, signed long);
			        else	w = va_arg(args, signed short);
		                v = (w < 0) ? -w : w;
				dr->prefix = (w < 0) ? "-" :
		                	(flags & _SIGNED) ? "+" :
					(flags & _BLANKER) ? " " : "";
		                }
		                if (flags & _PRECGIVEN) flags &= ~_PADZERO;
		                break;
		    case 'f':
		    case 'F':
		    case 'e':
		    case 'E':
		    case 'g':
		    case 'G':   
		    case 'r':
		    case 'R':
		    case 'y':
		    case 'Y':
		    		flags |= _FPCONV;
		                if (!(flags & _PRECGIVEN))
		                	dr->precision = 1;
		                dr->f = va_arg(args, float);
		                dr->prefix = (flags&_SIGNED) ? "+" :
		               		(flags&_BLANKER) ? " " : "";
		                break;
		    case 'c':   ch = va_arg(args, int);		// Or char ??????????
		    default:    width--;                        /* char width is 1       */
		                pre_padding(dr, p);
		                xputc(dr, ch, p);
		                charcount++;
		                post_padding(dr, p);
		                continue;
            }
            dr->width = width;
            dr->flags = flags;
            charcount += vcbprintf_display(p, ch, dr, v);
            continue;
        }
    }
    return charcount;
}

int _sputc(short ch, FILE *fp)
{
	char **op = (char **) fp;
	*((*op)++) = ch;
	return ch;
}

short sprintf(char *buff, const char *fmt, ...)
{
    char *sf = buff;
    va_list a;
    int length;
    va_start(a, fmt);
    asm ("nop\n nop");
    length = vcbprintf(_sputc, (FILE *)&sf, fmt, a);
    *(sf) = 0;
    va_end(a);
    return length;
}

int fputchar(short ch, FILE *p)
{
	char	Font = FontGetSys();
	short	Size = FontCharWidth(ch);
	short	x = CURRENT_POINT_X, y = CURRENT_POINT_Y;
	if ((ch == '\n') || (x + Size > (GET_XMAX+1)))
		{
		short	Height = (Font*2+6);	// NewLine
		y += Height;
		if (y + Height > GET_YMAX)
			{
			ScrRectScroll(&ScrRect, &ScrRect, Height, 0);
			y -= Height;
			}
		if (++PRINTF_LINE_COUNTER > 14)
			{
			ST_busy(2);		// Display 'Pause'
			ngetchx();		// Wait a key
			ST_busy(0);		// Return to normal mode
			PRINTF_LINE_COUNTER = 0;
			}
		x = 0;
		} 
	if (ch != '\n')
		{
		DrawChar(x, y, ch, 4);
		x+=Size;
		}
	CURRENT_POINT_X = x;
	CURRENT_POINT_Y = y;
	return ch;
}

short printf(const char *fmt, ...)
{
    va_list a;
    int length;
    va_start(a, fmt);
    length = vcbprintf(fputchar, NULL, fmt, a);
    va_end(a);
    return length;
}

void clrscr()
{
	CURRENT_POINT_X = 0;
	CURRENT_POINT_Y = 0;
	SHELL_SAVE_Y_POS = 0;
	PRINTF_LINE_COUNTER = 0;
	ClrScr();
	HELP_BEING_DISPLAYED = 1;
	ST_eraseHelp();	
}
