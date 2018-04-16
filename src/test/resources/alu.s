# Logical (shifted register) 64-bit
and	x15, x5, x0
bic	x12, x3, x0
orr	x16, x3, x0
orn	x1, x21, x0
eor	x5, x11, x0
eon	x1, x1, x0
and	x5, x8, x15, LSL #15
and	x3, x15, x20, LSR #15
and	x7, x1, x0, ASR #1

add	x5, x7, x30
sub	x2, x10, x0
