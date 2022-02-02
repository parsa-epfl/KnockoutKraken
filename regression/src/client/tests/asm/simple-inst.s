.section .text
.global _start
_start:
// x0: base register 0
// x1: base register 1
// x2: dest reg
  add x2, x0, x1
  add x2, x0, x1
  svc #0
