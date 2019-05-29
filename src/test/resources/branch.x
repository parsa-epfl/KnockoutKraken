
branch.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <main>:
   0:	00000014 	b	0 <fun>

0000000000000004 <offset1>:
   4:	c0000054 	b.eq	1c <offset2>  // b.none
   8:	a1000054 	b.ne	1c <offset2>  // b.any
   c:	82000054 	b.cs	1c <offset2>  // b.hs, b.nlast
  10:	62000054 	b.cs	1c <offset2>  // b.hs, b.nlast
  14:	43000054 	b.cc	1c <offset2>  // b.lo, b.ul, b.last
  18:	23000054 	b.cc	1c <offset2>  // b.lo, b.ul, b.last

000000000000001c <offset2>:
  1c:	44ffff54 	b.mi	4 <offset1>  // b.first
  20:	e5ffff54 	b.pl	1c <offset2>  // b.nfrst
  24:	a6000054 	b.vs	38 <offset3>
  28:	e7feff54 	b.vc	4 <offset1>
  2c:	88ffff54 	b.hi	1c <offset2>  // b.pmore
  30:	49000054 	b.ls	38 <offset3>  // b.plast
  34:	8afeff54 	b.ge	4 <offset1>  // b.tcont

0000000000000038 <offset3>:
  38:	6bfeff54 	b.lt	4 <offset1>  // b.tstop
  3c:	4cfeff54 	b.gt	4 <offset1>
  40:	2dfeff54 	b.le	4 <offset1>
  44:	0efeff54 	b.al	4 <offset1>
  48:	effdff54 	b.nv	4 <offset1>
