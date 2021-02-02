.text
.globl _start
_start:
  li sp, 0x0001FFFC
  li fp, 0x0001FFFC
  jal main

loop:
  j loop