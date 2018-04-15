# Logical (shifted register) 64-bit
and	w1, w11, w6
bic	w2, w12, w5
orr	w3, w13, w3
orn	w4, w15, w2
eor	w5, w11, w1
eon	w0, w1, w0
and	w0, w1, w0, LSL #15
and	w0, w1, w0, LSR #15
and	w0, w1, w0, ASR #15
