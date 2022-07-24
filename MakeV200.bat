@del pedromV2.tib
@del pedromV2.sav
@ttstrip ..\Preos\stdlib.9xz stdlib.bin
@a68k -t -i..\Preos\Src -iS pedrom.asm -lC:\temp\listing.txt -vV200
@del stdlib.bin
@maketib pedrom.o
@del pedrom.o
@ren pedrom.tib pedromV2.tib