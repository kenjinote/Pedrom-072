/******************************************************************************
*
* project name:    TIGCC Tools Suite
* file name:       asm_upck.c
* initial date:    30/08/2000
* authors:         albert@cs.tut.fi
*                  b.denis@libertysurf.fr
*                  thomas.nussbaumer@gmx.net
*
*
* description:     optimizated unpack routine
*                  (a generic C only version can be found in directory shared)
*
* -----------------------------------------------------------------------------
*
* based on code from Pasi 'Albert' Ojala, albert@cs.tut.fi
* heavily reduced to fit to the needs by thomas.nussbaumer@gmx.net
* assembler optimizations by b.denis@libertysurf.fr
*
* for details on the used algorithm see:
*
* http://www.cs.tut.fi/~albert/Dev/pucrunch/index.html
*
*
* $Id: asm_upck.c,v 1.2 2000/10/18 23:20:31 Thomas Nussbaumer Exp $
*
******************************************************************************/

#define ERRPCK_OKAY             0
#define ERRPCK_NOESCFOUND     248
#define ERRPCK_ESCBITS        249  
#define ERRPCK_MAXGAMMA       250
#define ERRPCK_EXTRALZP       251
#define ERRPCK_NOMAGIC        252 
#define ERRPCK_OUTBUFOVERRUN  253
#define ERRPCK_LZPOSUNDERRUN  254

int _tt_Decompress(unsigned char *src, unsigned char *dest);
#define UnPack _tt_Decompress

#define MAGIC_CHAR1     'T'
#define MAGIC_CHAR2     'P'

#define MAX_RLE_ENTRIES 31

// size = 16 bytes 
typedef struct {
    unsigned char origsize_lo; // original size lowbyte
    unsigned char origsize_hi; // original size highbyte
    unsigned char magic1;      // must be equal to MAGIC_CHAR1
    unsigned char magic2;      // must be equal to MAGIC_CHAR2
    unsigned char compsize_lo; // compressed size lowbyte
    unsigned char compsize_hi; // compressed size lowbyte
    unsigned char esc1;        // escape >> (8-escBits)
    unsigned char notused3;
    unsigned char notused4;
    unsigned char esc2;        // escBits
    unsigned char gamma1;      // maxGamma + 1
    unsigned char gamma2;      // (1<<maxGamma)
    unsigned char extralz;     // extraLZPosBits
    unsigned char notused1;
    unsigned char notused2;
    unsigned char rleentries;  // rleUsed
} PackedHeader;


#define GetUnPackedSize(p)  (unsigned int)((p)->origsize_lo | ((p)->origsize_hi << 8))
#define IsPacked(p)         ((p)->magic1 == MAGIC_CHAR1 && (p)->magic2 == MAGIC_CHAR2)

typedef struct { 
    unsigned char value[MAX_RLE_ENTRIES];
} RLEEntries;


//-----------------------------------------------------------------------------
// All globals and functions are prefixed with _tt_ by the intention to
// prevent possible name conflicts in the global namespace
//-----------------------------------------------------------------------------

// They are declared as extern so that the program can be in Flash ROM !

extern	unsigned char* _tt_InBuffer; // input buffer
extern	unsigned int   _tt_InMask  ; // input buffer byte mask


//---------------------------------------------------------------------------
// _tt_InMask is initialized with 0x80 and gets shifted right each time about
// 1 bit. CORRECT_IN_MASK will handle the shift right and if the mask becomes
// zero it will set it again to 0x80 + increments the inputbuffer
//---------------------------------------------------------------------------
#define CORRECT_IN_MASK(label)  asm("ror.b  #1,%0\n\t" \
                                "bcc.s  __inl_tt_cim_skip" #label "\n\t" \
                                "addq.l #1,%1\n" \
                                "__inl_tt_cim_skip" #label ":"\
                                : "=d" (_tt_InMask), "=g" (_tt_InBuffer) : "0" (_tt_InMask), "1" (_tt_InBuffer) );

// note: this one doesn't seem to improve speed (but the ror.b optimization in the assembly part does :)
// This is the only way I have found to have an auto incrementing __inl_tt_cim_skip0, ...1, ...2, etc. label :(
// Do you have any idea ?

//---------------------------------------------------------------------------
// returns >0 if next bit is set, otherwise 0
//---------------------------------------------------------------------------
#define NEXT_BIT_SET       (*_tt_InBuffer & _tt_InMask)

//=============================================================================
// get next 8 bits
//=============================================================================
unsigned int _tt_Get8Bit();
asm("
.globl _tt_Get8Bit
_tt_Get8Bit:
    lea     _tt_InBuffer,%a0
    movea.l (%a0),%a1           /* address of current byte */
    clr.w   %d0
    move.b  (%a1)+,%d0          /* anyway, read first byte */
    move.l  %a1,(%a0)           /* and increment pointer */
    move.w  _tt_InMask,%d1
    cmpi.b  #0x80,%d1
    bne.s   __inl_tries
    rts
__inl_tries:
    lsl.w   #8,%d0              /* v = *(_tt_InBuffer++) << 8; */
    move.b  (%a1),%d0           /* v |= *_tt_InBuffer; */
    /* bra.s    try08 */
    cmpi.b  #0x08,%d1
    bhi.s   __inl_try20
    bne.s   __inl_try02
    andi.w  #0x0fff,%d0
    lsr.w   #4,%d0
    rts
__inl_try40:
    andi.w  #0x7fff,%d0
    lsr.w   #7,%d0
    rts
__inl_try20:
    cmpi.b  #0x20,%d1
    bhi.s   __inl_try40
    bcs.s   __inl_try10
    andi.w  #0x3fff,%d0
    lsr.w   #6,%d0
    rts
__inl_try10:
    andi.w  #0x1fff,%d0
    lsr.w   #5,%d0
    rts
__inl_try04:
    andi.w  #0x07ff,%d0
    lsr.w   #3,%d0
    rts
__inl_try02:
    cmpi.b  #0x02,%d1
    bhi.s   __inl_try04
    bne.s   __inl_try01
    andi.w  #0x03ff,%d0
    lsr.w   #2,%d0
    rts
__inl_try01:
    andi.w  #0x01ff,%d0
    lsr.w   #1,%d0
    rts
");

//=============================================================================
// get next value from the input buffer
//=============================================================================
unsigned int _tt_GetValue(unsigned int maxgamma);
asm("
.globl _tt_GetValue
_tt_GetValue:
    move.l  %d3,-(%sp)
    clr.l   %d1                 /* i */
    move.w  8(%sp),%d0          /* maxgamma */
    move.l  _tt_InBuffer,%a0
    move.w  _tt_InMask,%d2

__inl_loop1:
    cmp.w   %d1,%d0
    bls.s   __inl_exitloop
    /* !(*_tt_InBuffer & _tt_InMask) */
    move.w  %d2,%d3
    and.b   (%a0),%d3
    /* if (!(_tt_InMask >>= 1)) _tt_InMask = 0x80,_tt_InBuffer++; */
    ror.b   #1,%d2          /* thx tom for the ror with carry trick ;) */
    bcc.s   __inl_samebyte1
    addq.l  #1,%a0
__inl_samebyte1:
    /* test if next bit set only now */
    tst.b   %d3
    beq.s   __inl_exitloop
    addq.w  #1,%d1
    bra.s   __inl_loop1
__inl_exitloop:
    move.l  %a0,_tt_InBuffer
    move.w  %d2,_tt_InMask
    moveq   #1,%d3
    lsl.w   %d1,%d3
    move.w  %d1,-(%sp)
    bsr.s       _tt_GetBits
    addq    #2,%sp
    or.w    %d3,%d0
    move.l  (%sp)+,%d3
    rts
");

//=============================================================================
// gets a number of bits from the input buffer
//=============================================================================
unsigned int _tt_GetBits(unsigned int bits);
asm("
.globl _tt_GetBits
_tt_GetBits:
    clr.w   %d0
    move.w  4(%sp),%d1
    beq.s   __inl_exit          /* just in case ;) */
    movea.l _tt_InBuffer,%a0    /* _tt_InBuffer, gcc makes it pc relative :) */
    movea.l %d3,%a1             /* use a1 to save d3: 4 cycles better than move.w %d3,-(%sp) */
    move.w  _tt_InMask,%d2      /* pc relative too :)) */
    subq.w  #1,%d1

__inl_loop0:
    lsl.w   #1, %d0
    /* (*_tt_InBuffer & _tt_InMask) */
    move.w  %d2,%d3
    and.b   (%a0),%d3
    beq.s   __inl_correct0
    addq.b  #1,%d0          /* here, bit #0 of %d0 is never set :) it's a four cycles gain :)) */
__inl_correct0:
    /* if (!(_tt_InMask >>= 1)) _tt_InMask = 0x80,_tt_InBuffer++; */
    ror.b   #1,%d2
    bcc.s   __inl_samebyte0
    addq.l  #1,%a0
__inl_samebyte0:
    dbra    %d1,__inl_loop0

    move.w  %d2,_tt_InMask      /* saves _tt_InMask */
    move.l  %a1,%d3             /* restores d3 (4 cycles again :) */
    move.l  %a0,_tt_InBuffer    /* saves _tt_InBuffer */
__inl_exit:
    rts
");

//=============================================================================
// the decompression routine
//
// using it is very simple: feed in a filled source array and a buffer which is
// large enough to hold the decompression result
//
// returns 0 if okay
//=============================================================================
int _tt_Decompress(unsigned char *src, unsigned char *dest)  {
    long           startEsc;
    char*          byteCodeVec;
    PackedHeader*  cth = (PackedHeader*)src;
    unsigned int   maxgamma1;
    unsigned int   maxgamma2;
    unsigned int   maxgamma8;
    unsigned int   escbits8;
    unsigned int   escbits;
    unsigned int   extralzposbits;
    unsigned int   maxgamma;
    unsigned char* outbuffer;
#ifdef ALL_CHECKS
    unsigned char* pend_in;
    unsigned char* pend_out;
#endif


    //---------------------------------------------------------------------
    // check if the magic markers exists. if they are not present we cannot
    // decompress this type of file
    //---------------------------------------------------------------------
    if (cth->magic1 != MAGIC_CHAR1 || cth->magic2 != MAGIC_CHAR2) return ERRPCK_NOMAGIC;

    startEsc       = cth->esc1;
    escbits        = cth->esc2;
    maxgamma       = cth->gamma1 - 1;
    extralzposbits = cth->extralz;

    maxgamma1 = 1 << maxgamma;
    maxgamma2 = 2 << maxgamma;
    maxgamma8 = 8 - maxgamma;
    escbits8  = 8 - escbits;


    if (escbits > 8)                                               return ERRPCK_ESCBITS;
    if (cth->gamma2 != maxgamma1 || maxgamma < 5 || maxgamma > 7)  return ERRPCK_MAXGAMMA;
    if (extralzposbits > 4)                                        return ERRPCK_EXTRALZP;


    byteCodeVec = &src[15];    // ??? shouldn't it start at 16 -- strange ???

    //--------------------------
    // initialize buffer globals
    //--------------------------
    outbuffer    = dest;
    _tt_InBuffer = src + sizeof(PackedHeader) + cth->rleentries;   // points at start of data
    _tt_InMask   = 0x80;


#ifdef ALL_CHECKS
    pend_in  = _tt_InBuffer + (((unsigned int)cth->compsize_lo) | ((unsigned int)(cth->compsize_hi << 8)));
    pend_out = dest + (((unsigned int)cth->origsize_lo) | ((unsigned int)(cth->origsize_hi << 8)));
#endif

    while (1) {
        int sel = startEsc;

#ifdef ALL_CHECKS
        if (outbuffer > pend_out)     return ERRPCK_OUTBUFOVERRUN;
        if (_tt_InBuffer  > pend_in)  return ERRPCK_NOESCFOUND;
#endif

        if (escbits) sel = _tt_GetBits(escbits);

        if (sel == startEsc) {
            unsigned int lzPos, lzLen = _tt_GetValue(maxgamma), i;
            unsigned int add = 0;

            if (lzLen != 1) {
                unsigned int lzPosHi = _tt_GetValue(maxgamma)-1, lzPosLo;

                if (lzPosHi == maxgamma2-2) {
                    if (lzLen > 3) {
                        add   = _tt_Get8Bit();
                        lzPos = _tt_Get8Bit() ^ 0xff;
                    }
                    else {
                        break;  // finish !!!
                    }
                }
                else {
                    if (extralzposbits) lzPosHi = (lzPosHi<<extralzposbits) | _tt_GetBits(extralzposbits);

                    lzPosLo = _tt_Get8Bit() ^ 0xff;
                    lzPos   = (lzPosHi<<8) | lzPosLo;
                }
            }
            else {
                if (NEXT_BIT_SET) {
                    unsigned int rleLen, byteCode, byte;
                    CORRECT_IN_MASK(0);

                    if (!NEXT_BIT_SET) {
                        unsigned int newEsc;
                        CORRECT_IN_MASK(1);

                        newEsc = _tt_GetBits(escbits);

                        *outbuffer++ = (startEsc<<escbits8) | _tt_GetBits(escbits8);
                        startEsc = newEsc;
#ifdef ALL_CHECKS
                        if (outbuffer > pend_out) return ERRPCK_OUTBUFOVERRUN;
#endif
                        continue;
                    }
                    //else {
                    CORRECT_IN_MASK(2);
                    //}
                    rleLen = _tt_GetValue(maxgamma);
                    if (rleLen >= maxgamma1) {
                        rleLen = ((rleLen-maxgamma1)<<maxgamma8) | _tt_GetBits(maxgamma8);
                        rleLen |= ((_tt_GetValue(maxgamma)-1)<<8);
                    }
                    byteCode = _tt_GetValue(maxgamma);
                    if (byteCode < 32) byte = byteCodeVec[byteCode];
                    else               byte = ((byteCode-32)<<3) | _tt_GetBits(3);

                    for (i=0; i<=rleLen; i++) *outbuffer++ = byte;
                    continue;
                }
                //else {
                CORRECT_IN_MASK(3);
                //}
                lzPos = _tt_Get8Bit() ^ 0xff;
            }

#ifdef ALL_CHECKS
            if (outbuffer + lzLen + 1 > pend_out) return ERRPCK_OUTBUFOVERRUN;
#endif
            for (i=0; i<=lzLen; i++) {
                *outbuffer = *(outbuffer - lzPos - 1) + add;
                outbuffer++;
            }
        }
        else {
            *outbuffer++ = (sel<<escbits8) | _tt_GetBits(escbits8);
#ifdef ALL_CHECKS
            if (outbuffer > pend_out) return ERRPCK_OUTBUFOVERRUN;
#endif
        }
    }

    return ERRPCK_OKAY;
}



//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// $Log: asm_upck.c,v $
// Revision 1.2  2000/10/18 23:20:31  Thomas Nussbaumer
// email address of Denis changed
//
// Revision 1.1  2000/08/30 20:01:00  Thomas Nussbaumer
// initial version
//
//
//
//
