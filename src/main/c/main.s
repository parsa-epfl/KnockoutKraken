
main.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <main>:
   0:	d10043ff 	sub	sp, sp, #0x10
   4:	12800040 	mov	w0, #0xfffffffd            	// #-3
   8:	b9000be0 	str	w0, [sp, #8]
   c:	b9000fff 	str	wzr, [sp, #12]
  10:	14000013 	b	5c <main+0x5c>
  14:	b9400fe0 	ldr	w0, [sp, #12]
  18:	11000402 	add	w2, w0, #0x1
  1c:	90000000 	adrp	x0, 258 <main+0x258>
  20:	91000003 	add	x3, x0, #0x0
  24:	b9800fe1 	ldrsw	x1, [sp, #12]
  28:	aa0103e0 	mov	x0, x1
  2c:	d37cec00 	lsl	x0, x0, #4
  30:	cb010000 	sub	x0, x0, x1
  34:	d37ef400 	lsl	x0, x0, #2
  38:	8b000060 	add	x0, x3, x0
  3c:	b9003402 	str	w2, [x0, #52]
  40:	b9400be1 	ldr	w1, [sp, #8]
  44:	b9400fe0 	ldr	w0, [sp, #12]
  48:	0b000020 	add	w0, w1, w0
  4c:	b9000be0 	str	w0, [sp, #8]
  50:	b9400fe0 	ldr	w0, [sp, #12]
  54:	11000400 	add	w0, w0, #0x1
  58:	b9000fe0 	str	w0, [sp, #12]
  5c:	b9400fe0 	ldr	w0, [sp, #12]
  60:	7100241f 	cmp	w0, #0x9
  64:	54fffd8d 	b.le	14 <main+0x14>
  68:	b9000fff 	str	wzr, [sp, #12]
  6c:	14000011 	b	b0 <main+0xb0>
  70:	90000000 	adrp	x0, 258 <main+0x258>
  74:	91000002 	add	x2, x0, #0x0
  78:	b9800fe1 	ldrsw	x1, [sp, #12]
  7c:	aa0103e0 	mov	x0, x1
  80:	d37cec00 	lsl	x0, x0, #4
  84:	cb010000 	sub	x0, x0, x1
  88:	d37ef400 	lsl	x0, x0, #2
  8c:	8b000040 	add	x0, x2, x0
  90:	39400c00 	ldrb	w0, [x0, #3]
  94:	2a0003e1 	mov	w1, w0
  98:	b9400be0 	ldr	w0, [sp, #8]
  9c:	0b010000 	add	w0, w0, w1
  a0:	b9000be0 	str	w0, [sp, #8]
  a4:	b9400fe0 	ldr	w0, [sp, #12]
  a8:	11000400 	add	w0, w0, #0x1
  ac:	b9000fe0 	str	w0, [sp, #12]
  b0:	b9400fe0 	ldr	w0, [sp, #12]
  b4:	7100241f 	cmp	w0, #0x9
  b8:	54fffdcd 	b.le	70 <main+0x70>
  bc:	b9400fe0 	ldr	w0, [sp, #12]
  c0:	7100141f 	cmp	w0, #0x5
  c4:	54000120 	b.eq	e8 <main+0xe8>  // b.none
  c8:	7100281f 	cmp	w0, #0xa
  cc:	54000080 	b.eq	dc <main+0xdc>  // b.none
  d0:	7100041f 	cmp	w0, #0x1
  d4:	54000120 	b.eq	f8 <main+0xf8>  // b.none
  d8:	1400000b 	b	104 <main+0x104>
  dc:	b9400be0 	ldr	w0, [sp, #8]
  e0:	11001400 	add	w0, w0, #0x5
  e4:	b9000be0 	str	w0, [sp, #8]
  e8:	b9400be0 	ldr	w0, [sp, #8]
  ec:	11000c00 	add	w0, w0, #0x3
  f0:	b9000be0 	str	w0, [sp, #8]
  f4:	14000005 	b	108 <main+0x108>
  f8:	b9400be0 	ldr	w0, [sp, #8]
  fc:	11000800 	add	w0, w0, #0x2
 100:	b9000be0 	str	w0, [sp, #8]
 104:	d503201f 	nop
 108:	b9400be0 	ldr	w0, [sp, #8]
 10c:	910043ff 	add	sp, sp, #0x10
 110:	d65f03c0 	ret
