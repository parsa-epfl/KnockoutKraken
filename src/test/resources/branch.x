
branch.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <main>:
   0:	14000000 	b	0 <fun>

0000000000000004 <offset1>:
   4:	540000c0 	b.eq	1c <offset2>  // b.none
   8:	540000a1 	b.ne	1c <offset2>  // b.any
   c:	54000082 	b.cs	1c <offset2>  // b.hs, b.nlast
  10:	54000062 	b.cs	1c <offset2>  // b.hs, b.nlast
  14:	54000043 	b.cc	1c <offset2>  // b.lo, b.ul, b.last
  18:	54000023 	b.cc	1c <offset2>  // b.lo, b.ul, b.last

000000000000001c <offset2>:
  1c:	540000e4 	b.mi	38 <offset3>  // b.first
  20:	540000c5 	b.pl	38 <offset3>  // b.nfrst
  24:	540000a6 	b.vs	38 <offset3>
  28:	54000087 	b.vc	38 <offset3>
  2c:	54000068 	b.hi	38 <offset3>  // b.pmore
  30:	54000049 	b.ls	38 <offset3>  // b.plast
  34:	5400002a 	b.ge	38 <offset3>  // b.tcont

0000000000000038 <offset3>:
  38:	54fffe6b 	b.lt	4 <offset1>  // b.tstop
  3c:	54fffe4c 	b.gt	4 <offset1>
  40:	54fffe2d 	b.le	4 <offset1>
  44:	54fffe0e 	b.al	4 <offset1>
  48:	54fffdef 	b.nv	4 <offset1>
