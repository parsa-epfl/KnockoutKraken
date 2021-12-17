.section .text
.global _start
_start:
  mov x0, #0
loop:
  cmp x0, #1
  b.ne loop
  svc #0
