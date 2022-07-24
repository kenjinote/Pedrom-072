/*
;* Extgraph Bitmap functions
;* Copyright (C) 2002 Thomas Nussbaumer
;*
;* Adaptated for PedroM - Operating System for Ti-89/Ti-92+/V200.
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
 
// Direct compilation ? or include from the test program ?
#ifndef	USE_KERNEL

#define _GENERIC_ARCHIVE
#define NULL 0

typedef union { 
 struct { unsigned char x0, y0, x1, y1; } xy; 
 unsigned long l; 
} SCR_RECT; 

enum Attrs{A_REVERSE,A_NORMAL,A_XOR,A_SHADED,A_REPLACE,A_OR,A_AND,A_THICK1,A_SHADE_V,A_SHADE_H,A_SHADE_NS,A_SHADE_PS};

extern		SCR_RECT ScrRect;
extern	char	*CURRENT_SCREEN;
extern	short	CURRENT_INCY;

#define	GET_XMAX (ScrRect.xy.x1)
#define	GET_YMAX (ScrRect.xy.y1)

void	BitmapInit(const SCR_RECT *rect, short *BitMap);
void	ScrRectFill (const SCR_RECT *rect, const SCR_RECT *clip, short Attr);
#define	ClrScr	ScreenClear

#else
char *CURRENT_SCREEN;		// Global positions
#define	GET_XMAX (ScrRect->xy.x1)
#define	GET_YMAX (ScrRect->xy.y1)
#endif

void	BitmapGet(const SCR_RECT *rect, short *BitMap)
{
	short x, y, h, w, bytewidth;
	unsigned char* dest;
	unsigned short FinalMask;
	
	BitmapInit(rect, BitMap);
	x = rect->xy.x0;
	y = rect->xy.y0;
	h = rect->xy.y1 - rect->xy.y0 + 1;
	w = rect->xy.x1 - rect->xy.x0 + 1;
	FinalMask = (0xFF >> (8-(w&7))) << (8-(w&7));
	bytewidth = /*((w-1)>>3)+1*/ (w+7) >> 3;
	dest = (unsigned char *) ((short *) BitMap+2);
	{
	    register unsigned char* addr  = (CURRENT_SCREEN)+(y*CURRENT_INCY+(x>>3));
	    register unsigned short mask1 = x & 7;
	    register unsigned short mask2;
	    register unsigned short lineoffset = CURRENT_INCY-bytewidth;
	    register          short loop;
	
	    if (mask1) 
	        {
	        mask2 = 8 - mask1;
	        for (;h;h--,addr+=lineoffset)
	            {
	            *dest = (*addr++) << mask1;
	            for (loop=1;loop<bytewidth;loop++)
	               {
	               *dest++ |= (*addr >> mask2);
	               *dest = (*addr++) << mask1;
	               }
	            *dest = (*dest | (*addr >> mask2)) & FinalMask;
	            dest++;
	            }
	         }
	    else {
	         for (;h;h--,addr+=lineoffset)
	            {
	            for (loop=0;loop<bytewidth;loop++) *dest++ = *addr++;
	            dest[-1] &= FinalMask;
	            }
	        }
	}
}

/*
 * Il manque le clipping
 */
void SpriteX8_or(short x,short y,short h,unsigned char* sprite,short bytewidth,void* dest) {
    register unsigned char* addr  = ((unsigned char*)dest)+(CURRENT_INCY*y+(x>>3));
    register unsigned short mask1 = x & 7; 
    register unsigned short mask2; 
    register unsigned short lineoffset = CURRENT_INCY-bytewidth; 
    register          short loop; 
    if (mask1) { 
        mask2 = 8 - mask1; 
        for (;h;h--,addr+=lineoffset) { 
            *addr++ |= *sprite >> mask1; 
            for (loop=1;loop<bytewidth;loop++) { 
               *addr |= ((*sprite++) << mask2); 
               *addr++ |= (*sprite >> mask1); 
            } 
            *addr |= (*sprite++ << mask2); 
        } 
    } else { 
        for (;h;h--,addr+=lineoffset) { 
            for (loop=0;loop<bytewidth;loop++) *addr++ |= *sprite++;
        } 
    }
}

void SpriteX8_and(short x,short y,short h,unsigned char* sprite,short bytewidth,void* dest) {
    register unsigned char* addr  = ((unsigned char*)dest)+(CURRENT_INCY*y+(x>>3));
    register unsigned short mask1 = x & 7; 
    register unsigned short mask2; 
    register unsigned short lineoffset = CURRENT_INCY-bytewidth; 
    register          short loop; 
    if (mask1) { 
        mask2 = 8 - mask1; 
        for (;h;h--,addr+=lineoffset) { 
            *addr++ &= *sprite >> mask1; 
            for (loop=1;loop<bytewidth;loop++) { 
               *addr &= ((*sprite++) << mask2); 
               *addr++ &= (*sprite >> mask1); 
            } 
            *addr &= (*sprite++ << mask2); 
        } 
    } else { 
        for (;h;h--,addr+=lineoffset) { 
            for (loop=0;loop<bytewidth;loop++) *addr++ &= *sprite++;
        } 
    }
}

void SpriteX8_xor(short x,short y,short h,unsigned char* sprite,short bytewidth,void* dest) {
    register unsigned char* addr  = ((unsigned char*)dest)+(CURRENT_INCY*y+(x>>3));
    register unsigned short mask1 = x & 7; 
    register unsigned short mask2; 
    register unsigned short lineoffset = CURRENT_INCY-bytewidth; 
    register          short loop; 
    if (mask1) { 
        mask2 = 8 - mask1; 
        for (;h;h--,addr+=lineoffset) { 
            *addr++ ^= *sprite >> mask1; 
            for (loop=1;loop<bytewidth;loop++) { 
               *addr ^= ((*sprite++) << mask2); 
               *addr++ ^= (*sprite >> mask1); 
            } 
            *addr ^= (*sprite++ << mask2); 
        } 
    } else { 
        for (;h;h--,addr+=lineoffset) { 
            for (loop=0;loop<bytewidth;loop++) *addr++ ^= *sprite++;
        } 
    }
}


void BitmapPut (short x, short y, const short *BitMap, const SCR_RECT *clip, short Attr)
{
	SCR_RECT	r;
	short	h, w;
	h = *(BitMap++);
	w = *(BitMap++);
	
	r.xy.x0 = x;
	r.xy.y0 = y;
	r.xy.y1 = y + h;
	r.xy.x1 = x + w;
	
	switch (Attr)
	{
	case	A_REVERSE:
		ScrRectFill(&r, clip, A_NORMAL);	
	case	A_XOR:
		SpriteX8_xor(x, y, h, (unsigned char *) BitMap, ((w-1)>>3)+1, CURRENT_SCREEN);
		break;
	case	A_REPLACE:
		ScrRectFill(&r, clip, A_REVERSE);
	case	A_NORMAL:
	case	A_OR:
		SpriteX8_or(x, y, h, (unsigned char *) BitMap, ((w-1)>>3)+1, CURRENT_SCREEN);
		break;	
	case	A_AND:
		SpriteX8_and(x, y, h, (unsigned char *) BitMap, ((w-1)>>3)+1, CURRENT_SCREEN);
	default:
		break;
	}
}
