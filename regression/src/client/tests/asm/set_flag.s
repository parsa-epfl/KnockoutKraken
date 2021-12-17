.section .text
.global _start
_start:
  mov x0, #1
  mov x1, #0
// x2: flag setting address
// x3: check for other flag address
loop:
  str x0, [x2] 
  ldr x1, [x3] 
  cmp x1, #0
  b.eq loop
  svc #0
