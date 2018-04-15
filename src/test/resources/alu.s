# Logical (shifted register) 64-bit
and	w15, w5, w0
bic	w12, w3, w0
orr	w16, w3, w0
orn	w1, w21, w0
eor	w5, w11, w0
eor	w1, w1, w0
and	w5, w8, w15, LSL #15
and	w3, w15, w20, LSR #15
and	w7, w1, w0, ASR #1

add	w5, w7, w30
sub	w2, w10, w0
