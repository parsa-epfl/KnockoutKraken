
alu.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <.text>:
   0:	af00008a 	and	x15, x5, x0
   4:	6c00208a 	bic	x12, x3, x0
   8:	700000aa 	orr	x16, x3, x0
   c:	a10220aa 	orn	x1, x21, x0
  10:	650100ca 	eor	x5, x11, x0
  14:	210020ca 	eon	x1, x1, x0
  18:	af0000ea 	ands	x15, x5, x0
  1c:	6c0020ea 	bics	x12, x3, x0
  20:	053d0f8a 	and	x5, x8, x15, lsl #15
  24:	e33d548a 	and	x3, x15, x20, lsr #15
  28:	2704808a 	and	x7, x1, x0, asr #1
  2c:	e5001e8b 	add	x5, x7, x30
  30:	420100cb 	sub	x2, x10, x0
  34:   22280091    add x2, x1, #10
  3c:	B5C23791    add x21,x21, #3568
  44:   94e23791    add x20,x20, #3576