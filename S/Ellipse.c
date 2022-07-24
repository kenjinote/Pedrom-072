/*
;* Ellipse function
;* Copyright (C) 2000 Allegro Team
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

typedef union { 
 struct { 
 unsigned char x0, y0, x1, y1; 
 } xy; 
 unsigned long l; 
} SCR_RECT; 

extern	SCR_RECT	CLIP_TEMP_RECT;

short SetCurAttr (short Attr); 
void SetCurClip (const SCR_RECT *clip); 
void DrawClipPix (short x, short y);

void DrawClipEllipse(short x, short y, short rx, short ry, const SCR_RECT *clip, short Attr)
{
   int ix, iy;
   int h, i, j, k;
   int oh, oi, oj, ok;
   
   Attr = SetCurAttr(Attr);
   SetCurClip(clip);
   
   if (rx < 1) 
      rx = 1; 

   if (ry < 1) 
      ry = 1;

   h = i = j = k = 0xFFFF;

   if (rx > ry) {
      ix = 0; 
      iy = rx * 64;

      do {
	 oh = h;
	 oi = i;
	 oj = j;
	 ok = k;

	 h = (ix + 32) >> 6; 
	 i = (iy + 32) >> 6;
	 j = (h * ry) / rx; 
	 k = (i * ry) / rx;

	 if (((h != oh) || (k != ok)) && (h < oi)) {
	    DrawClipPix(x+h, y+k); 
	    if (h) 
	       DrawClipPix(x-h, y+k);
	    if (k) {
	       DrawClipPix(x+h, y-k); 
	       if (h)
		  DrawClipPix(x-h, y-k);
	    }
	 }

	 if (((i != oi) || (j != oj)) && (h < i)) {
	    DrawClipPix(x+i, y+j); 
	    if (i)
	       DrawClipPix(x-i, y+j);
	    if (j) {
	       DrawClipPix(x+i, y-j); 
	       if (i)
		  DrawClipPix(x-i, y-j);
	    }
	 }

	 ix = ix + iy / rx; 
	 iy = iy - ix / rx;

      } while (i > h);
   } 
   else {
      ix = 0; 
      iy = ry * 64;

      do {
	 oh = h;
	 oi = i;
	 oj = j;
	 ok = k;

	 h = (ix + 32) >> 6; 
	 i = (iy + 32) >> 6;
	 j = (h * rx) / ry; 
	 k = (i * rx) / ry;

	 if (((j != oj) || (i != oi)) && (h < i)) {
	    DrawClipPix(x+j, y+i); 
	    if (j)
	       DrawClipPix(x-j, y+i);
	    if (i) {
	       DrawClipPix(x+j, y-i); 
	       if (j)
		  DrawClipPix(x-j, y-i);
	    }
	 }

	 if (((k != ok) || (h != oh)) && (h < oi)) {
	    DrawClipPix(x+k, y+h); 
	    if (k)
	       DrawClipPix(x-k, y+h);
	    if (h) {
	       DrawClipPix(x+k, y-h); 
	       if (k)
		  DrawClipPix(x-k, y-h);
	    }
	 }

	 ix = ix + iy / ry; 
	 iy = iy - ix / ry;

      } while(i > h);
   }
  SetCurAttr(Attr);
  SetCurClip(&CLIP_TEMP_RECT);
}
