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

#include "pedrom.h"

static const internal_bcd exp_range[] = {
  {0x0000,0x3fff, 0x43429448, 0x19032518}, // 1/ln(10): .4342944819032518
  {0x0000,0x4000, 0x23025851, 0x00000000}, // M1 2.302585100000000
  {0xffff,0x3ff8, 0x70059543, 0x15982008}  // M2 (ln(10)-M1): -.70059543159820085453156e-8
  };
static const internal_bcd sincos_range[] = {
  {0x0000,0x3FFf,0x31830988,0x61837907}, /* 1/pi to extended precision : .3183098861837907 */
  {0x0000,0x4000,0x31416015,0x62500000}, /* M1 = 3.1416015625 exactly */
  {0x8000,0x3Ffb,0x89089102,0x07000000}  /* M2 = pi-M1 to ext. prec. -.8908910207e-5*/
  };
static const internal_bcd tan_range[] = {
  {0x0000,0x3FFF,0x63661977,0x23675814}, /* 2/pi to extended precision */
  {0x0000,0x4000,0x15708007,0x81250000}, /* M1 = 1.57080078125 exactly */
  {0xFFFF,0x3FFA,0x44544551,0x03000000}  /* M2 = pi/2-M1 to ext. prec.: -.4454455103e-5 */
  };

static const poly_bcd tanp_poly = {
  4, {
  {0xFFFF,0x3FFB,0x17861707,0x34225442}, /* p3 = -0.17861707342254426711e-4 */
  {0x0000,0x3FFD,0x34248878,0x23589058}, /* p2 =  0.34248878235890589960e-2 */
  {0xFFFF,0x3FFF,0x13338350,0x00642196}, /* p1 = -0.13338350006421960681e+0 */
  {0x0000,0x4000,0x10000000,0x00000000}  /* p0 =  1.0 */
  }};
static const poly_bcd tanq_poly = {
  5, {
  {0x0000,0x3FF9,0x49819433,0x99378651}, /* q4 =  0.49819433993786512270e-6 */
  {0xFFFF,0x3FFC,0x31181531,0x90701002}, /* q3 = -0.31181531907010027307e-3 */
  {0x0000,0x3FFE,0x25663832,0x28944011}, /* q2 =  0.25663832289440112864e-1 */
  {0xFFFF,0x3FFF,0x46671683,0x33975529}, /* q1 = -0.46671683339755294240e+0 */
  {0x0000,0x4000,0x10000000,0x00000000}  /* q0 =  1.0 */
  }};  
static const poly_bcd exp_P = {
  3, {
  {0x0000,0x3ffb, 0x31555192, 0x76568464}, /* 2*p2 (p1=0.31555192765684646356e-4) */
  {0x0000,0x3ffd, 0x75753180, 0x15942277}, /* 2*p1 (p2=0.75753180159422776666e-2) */
  {0x0000,0x3fff, 0x25000000, 0x00000000}  /* 2*p0 (p0=0.25000000000000000000e+0) */
  }};
static const poly_bcd exp_Q = {
  4, {
  {0x0000,0x3ff9, 0x75104028, 0x39987004}, /* 2*q3 (q3=0.75104028399870046114e-6) */
  {0x0000,0x3ffc, 0x63121894, 0x37439850}, /* 2*q2 (q2=0.63121894374398503557e-3) */
  {0x0000,0x3ffe, 0xe8b9428e, 0xfecff592}, /* 2*q1 (q1=0.56817302698551221787e-1) */
  {0x0000,0x3fff, 0x50000000, 0x00000000}  /* 2*q0 (q0=0.50000000000000000000e-0) */
  }};
static const internal_bcd log2 = { 0x0000,0x3FFf, 0x69314718, 0x05599436}, /*.6931471805599436*/
			log2b = {0x0000,0x3Ff1, 0x16885250, 0x02014357 };/*.1688525002014357e-14*/
static const internal_bcd log10_f = {0x0000,0x4000, 0x23025850, 0x92994046},
			log10b_f = {0x0000, 0x3ff1, 0x31598200, 0x85453156};

static const poly_bcd loga_poly = {
	3, {
	{0xffff,0x3FFf,0x78561128,0x87491257},   /* a2 = -0.78956112887491257267e0 */
	{0x0000,0x4001,0x16383943,0x56302153},   /* a1 =  0.16383943563021534222e2 */
	{0xffff,0x4001,0x64124943,0x42374558}    /* a0 = -0.64124943423745581147e2 */
	}};
static const poly_bcd	logb_poly = {
	4, {
	{0x0000,0x4000,0x10000000,0x00000000},   /* b3 = 1.0 */  	
	{0xffff,0x4001,0x35667977,0x73903464},   /* b2 = -0.35667977739034646171e2 */
	{0x0000,0x4002,0x31203222,0x09192453},   /* b1 =  0.31203222091924532844e3 */
	{0xffff,0x4002,0x76949932,0x10849487}    /* b0 = -0.76949932108494879777e3 */
	}};
static const poly_bcd sincos_poly = {
  8, {
  {0x0000,0x3Ff1,0x27204790,0x95788885}, /* r8 in Cody & Waite : .2720479095788885e-14*/
  {0xffff,0x3Ff3,0x76429178,0x06891047}, /* r7 : -.7642917806891047e-12*/
  {0x0000,0x3Ff6,0x16058936,0x49037159}, /* r6 : .1605893649037159e-9*/
  {0xffff,0x3Ff8,0x25052106,0x79827458}, /* r5 : -.2505210679827458e-7*/
  {0x0000,0x3Ffa,0x27557319,0x21015276}, /* r4 : .2755731921015276e-5*/
  {0xffff,0x3FFc,0x19841269,0x84120184}, /* r3 : -.1984126984120184e-3*/
  {0x0000,0x3FFd,0x83333333,0x33333165}, /* r2 : .8333333333333165e-2*/
  {0xffff,0x3FFf,0x16666666,0x66666667}  /* r1 : -.1666666666666667*/
  }};
static const poly_bcd asnacsp_poly = {
  5, {
  {0xFFFF,0x3FFF,0x69674573,0x44735064}, /* p5 = -0.69674573447350646411e0 */
  {0x0000,0x4001,0x10152522,0x23380646}, /* p4 =  0.10152522233806463645e2 */
  {0xFFFF,0x4001,0x39688862,0x99750487}, /* p3 = -0.39688862997504877339e2 */
  {0x0000,0x4001,0x57208227,0x87789173}, /* p2 =  0.57208227877891731407e2 */
  {0xFFFF,0x4001,0x27368494,0x52416425}  /* p1 = -0.27368494524164255994e2 */
  }};
static const poly_bcd asnacsq_poly = {
  6, {
  {0x0000,0x4000,0x10000000,0x00000000}, /* q4 = -0.23823859153670238830e2 */  	
  {0xFFFF,0x4001,0x23823859,0x15367023}, /* q4 = -0.23823859153670238830e2 */
  {0x0000,0x4002,0x15095270,0x84103060}, /* q3 =  0.15095270841030604719e3 */
  {0xFFFF,0x4002,0x38186303,0x36175014}, /* q2 = -0.38186303361750149284e3 */
  {0x0000,0x4002,0x41714430,0x24826041}, /* q1 =  0.41714430248260412556e3 */
  {0xFFFF,0x4002,0x16421096,0x71449856}  /* q0 = -0.16421096714498560795e3 */
  }};

// In: FloatReg1 / Out: FloatReg1
void	FloatExp(void)
{
  range_red g;
  internal_bcd ix = FloatReg1;
  
  // The algorithm used in what follows comes from Cody & Waite, chapter 6.
  g = FRangeRedByMod(FloatReg1,exp_range);
  if (g.n > -(1<<14) || g.n <= (1<<14))
	{
	if (FIsGreaterThan10Pow(g.x, -20))
		{
		internal_bcd z,gpz;
		z = FSquare(g.x);
		gpz = FMul(FPolyEval(&exp_P, z), g.x);
		FloatReg1 = FAdd(FDiv(gpz, FSub(FPolyEval(&exp_Q, z), gpz)), FloatHalf);
		FloatReg1.exponent += g.n/*+1*/;
		return;
	  	}
	else	{
		FloatReg1 = FAdd(g.x, FloatOne);	// 1+x if x very small
		FloatReg1.exponent += g.n /*+ 1*/;
		return;
		}
	}
  else  {
	if (FIsPos(ix))
		FloatReg1 = FloatPosInfinity;
	else	FloatReg1 = FloatZero;
	}
}

// sign = 0 or 0xFFFF
void	_sincos(int sign)
{
  range_red g;
  
  g = FRangeRedByMod(FloatReg1, sincos_range);
  if ((g.n)&1)
  	sign ^= 0xFFFF;

  if (FIsGreaterThan10Pow(g.x,-10))
	FloatReg1 = FSpike(FPolyEval(&sincos_poly, FSquare(g.x)), g.x);
  else	FloatReg1 = g.x;
  
  if (sign)
  	FloatReg1.sign ^= 0xFFFF;
  return;
}

void	FloatSin(void)
{
  if (!FIsZero(FloatReg1))
	{
	register int sign=FloatReg1.sign;
	FloatReg1.sign = 0;
	_sincos(sign);
	}
}

void	FloatCos(void)
{
  if (!FIsZero(FloatReg1))
	{
	FloatReg1.sign = 0;
	FloatReg2 = FloatPiDiv2;
	FloatAdd();
	_sincos(0);
	}
  else	FloatReg1 = FloatOne;
}


void	FloatTan(void)
{
  range_red range;

  if (FIsZero(FloatReg1))	return;  
  range = FRangeRedByMod(FloatReg1, tan_range);

  if (FIsGreaterThan10Pow(range.x, -10))
	{
	internal_bcd g, xnum, xden;
	g = FSquare(range.x);
	xnum = FMul( FPolyEval(&tanp_poly, g), range.x);
	xden = FPolyEval(&tanq_poly, g);
	if (range.n&1)
		xden.sign ^= 0xFFFF;
	FloatReg1 = FDiv(xnum, xden);
	}
   else if (range.n & 1)
	FloatReg1 = FDiv(FloatMinusOne, range.x);
}

void	_asnacs(int acs_flag)
{
  internal_bcd ix,g;
  int sign,flags;

  sign = FloatReg1.sign;
  FloatReg1.sign = 0;
    
  if (FUCmp(FloatReg1,FloatHalf) >=0 )
	{
	if (FIsGreaterThan10Pow(FloatReg1,0))
		goto error;
	flags=acs_flag ? sign ? 0xe : 0x0 : sign ? 0xd : 0xc;
	g = FMul(FSub(FloatOne, FloatReg1), FloatHalf);
	ix = FMult2(FSqrt(g));
	}
  else	{
	flags=acs_flag ? sign ? 0x4 : 0xc : sign ? 0x1 : 0x0;
	if (!FIsGreaterThan10Pow(ix,-10))
		goto very_small;
	ix = FloatReg1;
	g = FSquare(FloatReg1);
	}
    FloatReg1 = FSpike(FDiv(FPolyEval(&asnacsq_poly, g), FPolyEval(&asnacsp_poly, g)), ix);
    
very_small:
    if (flags & 8)
    	FloatReg1.sign ^= 0xFFFF;
    if (flags & 0x4)
	{            
	if (flags & 0x2)
		FloatReg1 = FAdd(FloatReg1, FloatPi);
	else	FloatReg1 = FAdd(FloatReg1, FloatPiDiv2);
	}
    if (flags & 0x1)
    	FloatReg1.sign ^= 0xFFFF;
    return;

error:
	FloatReg1 = FloatNAN;
}

void	FloatAsin(void)
{
  if (FIsZero(FloatReg1))
  	return;
  else	_asnacs(0);
}

void	FloatAcos(void)
{
  _asnacs(1);
}

void	FloatPow(void)
{
  internal_bcd x,y;
  long	ny;
  unsigned int sign=0;

  if ((FloatReg1.exponent | FloatReg2.exponent) == 0x7FFF)	return;
  else if (FloatReg2.exponent == 0x2000)
  	{
  	FloatReg1 = FloatOne;		// x^0 = 1
 	return;
	}
  else if (FloatReg1.exponent == 0x2000)
	return;

  x = FloatReg1; y= FloatReg2;
  if (FCmp(FCeil(y),y)==0 )
      {
      ny = FFloor(y);
      // Integer power : special cases 
      if (FCmp(x,FloatOne)==0 || ny==1)	{FloatReg1 = x; return;}
      if (FCmp(x,FloatMinusOne)==0 && !(ny & 1)) {x.sign^=0xFFFF; FloatReg1 = x; return;}
      if (ny==2) {FloatReg1 = FMul(x, x); return;}

      if (x.sign)
      	{
	sign = (ny&1 ? 0xFFFF : 0);
	x.sign = 0;
	}

      if ((ny<0 ? -ny : ny) < (1L<<16)) //abs(ny)<2^16
	{ 
	internal_bcd acc;
	unsigned long bit=1L<<16;
	
	// Use a repeated multiplication/division for iy in [-2^16..2^16]
	if (ny>0) 
		{
	          while (!(ny & bit)) bit>>=1;
	          acc=x; bit>>=1;
	          while (bit)
			{
			acc=FSquare(acc);
			if (ny & bit) acc=FMul(acc,x);
			bit>>=1;
			}
		}
	else	{
	          ny=-ny;
	          while (!(ny & bit)) bit>>=1;
	          acc = FDiv(FloatOne, x); bit>>=1;
	          while (bit)
			{
			acc=FSquare(acc);
			if (ny & bit) acc=FDiv(acc,x);
			bit>>=1;
			}
	        }
	acc.sign ^=sign;
	FloatReg1 = acc;
	return;
	}
     }
  /* A number to a regular power... implement using log and exp */
  FloatReg1 = FExp(FMul(FLn(x),y));
  return;
}

void	FloatLn(void)
{
  internal_bcd z,w,r,f;
  short n;

  n = FloatReg1.exponent - 0x3FFF;
  FloatReg1.exponent = 0x3FFF;
  f = FloatReg1;
  
  if (FCmp(f, FloatSqrtHalf) > 0)
	z = FDiv(FAdd(FloatMinusOne, f), FSpike(f, FloatHalf));
  else	{
	n--;
	w = FAdd(FloatMinusHalf,f);
	z = FDiv(w, FSpike(w, FloatHalf));
	}

  w = FSquare(z);
  r = FSpike(FDiv(FPolyEval(&loga_poly,w),FPolyEval(&logb_poly, w)),z);

  if (n)
	{
	internal_bcd xn,v1,t1;
	
	xn = FFInt(n);
	v1 = FMul(xn, log10_f);
	t1 = FAdd(v1, r);
	r  = FAdd(FAdd(FMul(xn,log10b_f),FSub(FSub(t1,v1),r)),t1);
	}    
  FloatReg1 = r;
  return;
}
