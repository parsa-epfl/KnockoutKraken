
decode.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <.text>:
   0:	0a060161 	and	w1, w11, w6
   4:	0a250182 	bic	w2, w12, w5
   8:	2a0301a3 	orr	w3, w13, w3
   c:	2a2201e4 	orn	w4, w15, w2
  10:	4a010165 	eor	w5, w11, w1
  14:	4a200020 	eon	w0, w1, w0
  18:	0a003c20 	and	w0, w1, w0, lsl #15
  1c:	0a403c20 	and	w0, w1, w0, lsr #15
  20:	0a803c20 	and	w0, w1, w0, asr #15
