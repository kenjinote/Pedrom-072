@del pedrom89.tib
@del pedrom89.sav
@ttstrip ..\Preos\stdlib.9xz stdlib.bin
@a68k -t -i..\Preos\Src -iS pedrom.asm -lC:\temp\listing.txt -vTI89
@del stdlib.bin
@maketib pedrom.o
@del pedrom.o
@ren pedrom.tib pedrom89.tib