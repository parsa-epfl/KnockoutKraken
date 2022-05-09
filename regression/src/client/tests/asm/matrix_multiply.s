.section .text
.global _start
_start:
mm:
  mov     x9, x1
  mov     x8, x2
  add     x10, x2, 4096
  mov     x6, x0
  add     x7, x1, 64
.L2:
  mov     x5, x9
  mov     x4, x8
.L4:
  mov     x1, 0
  mov     w0, 0
  strb    wzr, [x4]
.L3:
  lsl     x3, x1, 6
  ldrb    w2, [x6, x1]
  add     x1, x1, 1
  ldrb    w3, [x5, x3]
  # w3 * w2 to w11 
  mul     w11, w2, w3
  # accumulate w11 to w0.
  add     w0, w11, w0
  and     w0, w0, 255
  strb    w0, [x4]
  cmp     x1, 64
  bne     .L3
  add     x5, x5, 1
  add     x4, x4, 1
  cmp     x5, x7
  bne     .L4
  add     x8, x8, 64
  add     x6, x6, 64
  cmp     x8, x10
  bne     .L2
  mov     w0, 0
  svc #0
