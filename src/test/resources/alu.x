
alu.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <.text>:
   0:	0a000020 	and	w0, w1, w0
   4:	0a200020 	bic	w0, w1, w0
   8:	2a000020 	orr	w0, w1, w0
   c:	2a200020 	orn	w0, w1, w0
  10:	4a000020 	eor	w0, w1, w0
  14:	4a000020 	eor	w0, w1, w0
  18:	0a003c20 	and	w0, w1, w0, lsl #15
  1c:	0a403c20 	and	w0, w1, w0, lsr #15
  20:	0a803c20 	and	w0, w1, w0, asr #15
  24:	0b000020 	add	w0, w1, w0
  28:	4b000020 	sub	w0, w1, w0
