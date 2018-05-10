.text

main:
	b	fun

offset1:
	b.eq  offset2
	b.ne  offset2
	b.cs  offset2
	b.hs  offset2
	b.cc  offset2
	b.lo  offset2
offset2:
	b.mi	offset3
	b.pl	offset3
	b.vs	offset3
	b.vc	offset3
	b.hi	offset3
	b.ls	offset3
	b.ge	offset3
offset3:
	b.lt	offset1
	b.gt	offset1
	b.le	offset1
	b.al	offset1
	b.nv	offset1
