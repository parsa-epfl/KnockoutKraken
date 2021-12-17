.section .text
.global _start
_start:
  mov x0, #0
  mov x1, #0
loop:
  add x0, x0, #1
  cmp x0, #10
  b.ne loop
  str x0, [x1]
  svc #0
  
