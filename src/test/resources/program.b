
program.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <main>:
   0:	d100c3ff 	sub	sp, sp, #0x30
   4:	52801e00 	mov	w0, #0xf0                  	// #240
   8:	b9002fe0 	str	w0, [sp, #44]
   c:	528001e0 	mov	w0, #0xf                   	// #15
  10:	b9002be0 	str	w0, [sp, #40]
  14:	b9402fe0 	ldr	w0, [sp, #44]
  18:	b90027e0 	str	w0, [sp, #36]
  1c:	b9402fe0 	ldr	w0, [sp, #44]
  20:	b90023e0 	str	w0, [sp, #32]
  24:	b9402fe0 	ldr	w0, [sp, #44]
  28:	b9001fe0 	str	w0, [sp, #28]
  2c:	b9402fe0 	ldr	w0, [sp, #44]
  30:	b9001be0 	str	w0, [sp, #24]
  34:	b9402fe0 	ldr	w0, [sp, #44]
  38:	b90017e0 	str	w0, [sp, #20]
  3c:	b9402fe0 	ldr	w0, [sp, #44]
  40:	b90013e0 	str	w0, [sp, #16]
  44:	b9402fe0 	ldr	w0, [sp, #44]
  48:	b9000fe0 	str	w0, [sp, #12]
  4c:	b9402fe0 	ldr	w0, [sp, #44]
  50:	b9000be0 	str	w0, [sp, #8]
  54:	b94027e1 	ldr	w1, [sp, #36]
  58:	b9402be0 	ldr	w0, [sp, #40]
  5c:	0a000020 	and	w0, w1, w0
  60:	b90027e0 	str	w0, [sp, #36]
  64:	b9402be0 	ldr	w0, [sp, #40]
  68:	2a2003e0 	mvn	w0, w0
  6c:	b94023e1 	ldr	w1, [sp, #32]
  70:	0a000020 	and	w0, w1, w0
  74:	b90023e0 	str	w0, [sp, #32]
  78:	b9401fe1 	ldr	w1, [sp, #28]
  7c:	b9402be0 	ldr	w0, [sp, #40]
  80:	2a000020 	orr	w0, w1, w0
  84:	b9001fe0 	str	w0, [sp, #28]
  88:	b9402be0 	ldr	w0, [sp, #40]
  8c:	2a2003e0 	mvn	w0, w0
  90:	b9401be1 	ldr	w1, [sp, #24]
  94:	2a000020 	orr	w0, w1, w0
  98:	b9001be0 	str	w0, [sp, #24]
  9c:	b94017e1 	ldr	w1, [sp, #20]
  a0:	b9402be0 	ldr	w0, [sp, #40]
  a4:	4a000020 	eor	w0, w1, w0
  a8:	b90017e0 	str	w0, [sp, #20]
  ac:	b9402be0 	ldr	w0, [sp, #40]
  b0:	2a2003e0 	mvn	w0, w0
  b4:	b94013e1 	ldr	w1, [sp, #16]
  b8:	4a000020 	eor	w0, w1, w0
  bc:	b90013e0 	str	w0, [sp, #16]
  c0:	b9400fe1 	ldr	w1, [sp, #12]
  c4:	b9402be0 	ldr	w0, [sp, #40]
  c8:	0b000020 	add	w0, w1, w0
  cc:	b9000fe0 	str	w0, [sp, #12]
  d0:	b9400be1 	ldr	w1, [sp, #8]
  d4:	b9402be0 	ldr	w0, [sp, #40]
  d8:	4b000020 	sub	w0, w1, w0
  dc:	b9000be0 	str	w0, [sp, #8]
  e0:	52800000 	mov	w0, #0x0                   	// #0
  e4:	9100c3ff 	add	sp, sp, #0x30
  e8:	d65f03c0 	ret

program.o:     file format elf64-littleaarch64


Disassembly of section .text:

0000000000000000 <main>:
   0:	d100c3ff 	sub	sp, sp, #0x30
   4:	52801e00 	mov	w0, #0xf0                  	// #240
   8:	b9002fe0 	str	w0, [sp, #44]
   c:	528001e0 	mov	w0, #0xf                   	// #15
  10:	b9002be0 	str	w0, [sp, #40]
  14:	b9402fe0 	ldr	w0, [sp, #44]
  18:	b90027e0 	str	w0, [sp, #36]
  1c:	b9402fe0 	ldr	w0, [sp, #44]
  20:	b90023e0 	str	w0, [sp, #32]
  24:	b9402fe0 	ldr	w0, [sp, #44]
  28:	b9001fe0 	str	w0, [sp, #28]
  2c:	b9402fe0 	ldr	w0, [sp, #44]
  30:	b9001be0 	str	w0, [sp, #24]
  34:	b9402fe0 	ldr	w0, [sp, #44]
  38:	b90017e0 	str	w0, [sp, #20]
  3c:	b9402fe0 	ldr	w0, [sp, #44]
  40:	b90013e0 	str	w0, [sp, #16]
  44:	b9402fe0 	ldr	w0, [sp, #44]
  48:	b9000fe0 	str	w0, [sp, #12]
  4c:	b9402fe0 	ldr	w0, [sp, #44]
  50:	b9000be0 	str	w0, [sp, #8]
  54:	b94027e1 	ldr	w1, [sp, #36]
  58:	b9402be0 	ldr	w0, [sp, #40]
  5c:	0a000020 	and	w0, w1, w0
  60:	b90027e0 	str	w0, [sp, #36]
  64:	b9402be0 	ldr	w0, [sp, #40]
  68:	2a2003e0 	mvn	w0, w0
  6c:	b94023e1 	ldr	w1, [sp, #32]
  70:	0a000020 	and	w0, w1, w0
  74:	b90023e0 	str	w0, [sp, #32]
  78:	b9401fe1 	ldr	w1, [sp, #28]
  7c:	b9402be0 	ldr	w0, [sp, #40]
  80:	2a000020 	orr	w0, w1, w0
  84:	b9001fe0 	str	w0, [sp, #28]
  88:	b9402be0 	ldr	w0, [sp, #40]
  8c:	2a2003e0 	mvn	w0, w0
  90:	b9401be1 	ldr	w1, [sp, #24]
  94:	2a000020 	orr	w0, w1, w0
  98:	b9001be0 	str	w0, [sp, #24]
  9c:	b94017e1 	ldr	w1, [sp, #20]
  a0:	b9402be0 	ldr	w0, [sp, #40]
  a4:	4a000020 	eor	w0, w1, w0
  a8:	b90017e0 	str	w0, [sp, #20]
  ac:	b9402be0 	ldr	w0, [sp, #40]
  b0:	2a2003e0 	mvn	w0, w0
  b4:	b94013e1 	ldr	w1, [sp, #16]
  b8:	4a000020 	eor	w0, w1, w0
  bc:	b90013e0 	str	w0, [sp, #16]
  c0:	b9400fe1 	ldr	w1, [sp, #12]
  c4:	b9402be0 	ldr	w0, [sp, #40]
  c8:	0b000020 	add	w0, w1, w0
  cc:	b9000fe0 	str	w0, [sp, #12]
  d0:	b9400be1 	ldr	w1, [sp, #8]
  d4:	b9402be0 	ldr	w0, [sp, #40]
  d8:	4b000020 	sub	w0, w1, w0
  dc:	b9000be0 	str	w0, [sp, #8]
  e0:	52800000 	mov	w0, #0x0                   	// #0
  e4:	9100c3ff 	add	sp, sp, #0x30
  e8:	d65f03c0 	ret
