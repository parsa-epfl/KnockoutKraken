.section .text
.global _start
_start:
  mov x0, #0
  mov x1, #0
// x2: memory address loads
// x3: memory address stores
// x4: offset between words
loop:
  ldp w0, w1, [x2]
  stp w0, w1, [x3]
  add x2, x2, x4
  add x3, x3, x4
  svc #0
  ldp x0, x1, [x2]
  stp x0, x1, [x3]
  add x2, x2, x4
  add x3, x3, x4
  svc #0
