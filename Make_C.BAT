@cd S
@tigcc -S -O2 -fomit-frame-pointer -Wall side.c -DPEDROM -DTI92P
@ren side.s side92.s
@tigcc -S -O2 -fomit-frame-pointer -Wall side.c -DPEDROM -DTI89
@ren side.s side89.s
@tigcc -S -O2 -fomit-frame-pointer -Wall printf.c
@tigcc -S -O2 -fomit-frame-pointer -Wall md5.c
@tigcc -S -O2 -fomit-frame-pointer -Wall bitmap.c
@tigcc -S -O2 -fomit-frame-pointer -Wall clipline.c
@tigcc -S -O2 -fomit-frame-pointer -Wall ellipse.c
@tigcc -S -O2 -fomit-frame-pointer -Wall unpack.c
@tigcc -S -O2 -fomit-frame-pointer -Wall float.c
@s2asm printf.s printf.asm LPrt 10
@s2asm md5.s md5.asm LMd5 10
@s2asm bitmap.s bitmap.asm LBitMap 10
@s2asm clipline.s clipline.asm LCLine 10
@s2asm ellipse.s ellipse.asm LElli 10
@s2asm unpack.s unpack.asm LPack 10
@s2asm side92.s side92.asm LSide 10
@s2asm side89.s side89.asm LSide 10
@s2asm float.s float.asm LFlt 10
@del *.s
@cd ..