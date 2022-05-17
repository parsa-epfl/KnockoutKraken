# Sort an array with 4k ints, and the starting address is stored in x0, in place.
# Shanqing is too stupid to write the quick sort :(. 
.section .text
.global _start
_start:
selectSort:
  mov     x4, x0
  mov     x5, 0
.L4:
  ldrb    w6, [x4, x5]
  mov     w3, w5
  mov     x7, x5
  add     x5, x5, 1
  mov     x0, x5
  mov     w1, w6
.L3:
  ldrb    w2, [x4, x0]
  cmp     w1, w2
  csel    w3, w0, w3, hi
  add     x0, x0, 1
  csel    w1, w1, w2, ls
  cmp     w0, 16
  bne     .L3
  strb    w6, [x4, w3, sxtw]
  strb    w1, [x4, x7]
  cmp     x5, 15
  bne     .L4
  svc     #0
