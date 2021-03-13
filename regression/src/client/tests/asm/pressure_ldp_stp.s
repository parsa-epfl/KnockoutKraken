.section .text
.global _start
_start:
  svc #0 // trigger transplant as first instruction to check correct startup and stop of multiple threads
  mov x0, #0
  mov x1, #0
  mov x2, #0
// x3: base address for load array
// x4: base address for store array
// x5: array size in bytes
loop:
  ldp x0, x1, [x3] // Get addr value
  stp x0, x1, [x4] // Store incremented value
  add x3, x3, #16 // Jump to next 128bytes
  add x4, x4, #16 // Jump to next 128bytes
  add x2, x2, #16
  cmp x2, x5
  b.ne loop
  svc #0
