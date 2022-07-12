.section .text
.global _start
_start:
  svc #0 // trigger transplant as first instruction to check correct startup and stop of multiple threads
  mov x9, #0
  mov x10, #0
  mov x11, #0
// x8: base address for array
// x9: memory size
loop:
  ldp x0, x1, [x8] // Get addr value
  ldp x2, x3, [x8, #16] // Get addr value
  ldp x4, x5, [x8, #32] // Get addr value
  ldp x6, x7, [x8, #48] // Get addr value
  stp x0, x1, [x8] // Store incremented value
  stp x2, x3, [x8, #16] // Store incremented value
  stp x4, x5, [x8, #32] // Store incremented value
  stp x6, x7, [x8, #48] // Store incremented value
  add x8, x8, #64// Jump to next 128bytes
  cmp x8, x9
  b.ne loop
  mov x8, #0
  b loop
  svc #0
