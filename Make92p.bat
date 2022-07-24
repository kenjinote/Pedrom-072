@del pedrom92.tib
@del pedrom92.sav
@ttstrip ..\Preos\stdlib.9xz stdlib.bin
@a68k -t -i..\Preos\Src -iS pedrom.asm -lC:\temp\listing.txt -vTI92P
@del stdlib.bin
@maketib pedrom.o
@del pedrom.o
@ren pedrom.tib pedrom92.tib