# Logical (shifted register) 64-bit
and	w0, w1, w0
bic w0, w1, w0
orr	w0, w1, w0
orn	w0, w1, w0
eor	w0, w1, w0
eor	w0, w1, w0
and	w0, w1, w0, LSL #15
and	w0, w1, w0, LSR #15
and	w0, w1, w0, ASR #15

add	w0, w1, w0
sub	w0, w1, w0
