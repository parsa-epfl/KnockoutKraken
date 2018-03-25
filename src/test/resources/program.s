	.arch armv8-a
	.file	"program.c"
	.text
.Ltext0:
	.align	2
	.global	main
	.type	main, %function
main:
.LFB0:
	.file 1 "program.c"
	.loc 1 8 0
	.cfi_startproc
	sub	sp, sp, #48
	.cfi_def_cfa_offset 48
	.loc 1 9 0
	mov	w0, 240
	str	w0, [sp, 44]
	.loc 1 10 0
	mov	w0, 15
	str	w0, [sp, 40]
	.loc 1 11 0
	ldr	w0, [sp, 44]
	str	w0, [sp, 36]
	ldr	w0, [sp, 44]
	str	w0, [sp, 32]
	ldr	w0, [sp, 44]
	str	w0, [sp, 28]
	ldr	w0, [sp, 44]
	str	w0, [sp, 24]
	ldr	w0, [sp, 44]
	str	w0, [sp, 20]
	ldr	w0, [sp, 44]
	str	w0, [sp, 16]
	.loc 1 12 0
	ldr	w0, [sp, 44]
	str	w0, [sp, 12]
	ldr	w0, [sp, 44]
	str	w0, [sp, 8]
	.loc 1 14 0
	ldr	w1, [sp, 36]
	ldr	w0, [sp, 40]
	and	w0, w1, w0
	str	w0, [sp, 36]
	.loc 1 15 0
	ldr	w0, [sp, 40]
	mvn	w0, w0
	ldr	w1, [sp, 32]
	and	w0, w1, w0
	str	w0, [sp, 32]
	.loc 1 16 0
	ldr	w1, [sp, 28]
	ldr	w0, [sp, 40]
	orr	w0, w1, w0
	str	w0, [sp, 28]
	.loc 1 17 0
	ldr	w0, [sp, 40]
	mvn	w0, w0
	ldr	w1, [sp, 24]
	orr	w0, w1, w0
	str	w0, [sp, 24]
	.loc 1 18 0
	ldr	w1, [sp, 20]
	ldr	w0, [sp, 40]
	eor	w0, w1, w0
	str	w0, [sp, 20]
	.loc 1 19 0
	ldr	w0, [sp, 40]
	mvn	w0, w0
	ldr	w1, [sp, 16]
	eor	w0, w1, w0
	str	w0, [sp, 16]
	.loc 1 20 0
	ldr	w1, [sp, 12]
	ldr	w0, [sp, 40]
	add	w0, w1, w0
	str	w0, [sp, 12]
	.loc 1 21 0
	ldr	w1, [sp, 8]
	ldr	w0, [sp, 40]
	sub	w0, w1, w0
	str	w0, [sp, 8]
	.loc 1 22 0
	ldr	w0, [sp, 8]
	cmp	w0, 0
	beq	.L2
	.loc 1 22 0 is_stmt 0 discriminator 1
	mov	w0, 1
	b	.L3
.L2:
	.loc 1 24 0 is_stmt 1
	mov	w0, 0
.L3:
	.loc 1 25 0
	add	sp, sp, 48
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
.Letext0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.4byte	0xdb
	.2byte	0x4
	.4byte	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.4byte	.LASF0
	.byte	0xc
	.4byte	.LASF1
	.4byte	.LASF2
	.8byte	.Ltext0
	.8byte	.Letext0-.Ltext0
	.4byte	.Ldebug_line0
	.uleb128 0x2
	.4byte	.LASF3
	.byte	0x1
	.byte	0x7
	.4byte	0xd7
	.8byte	.LFB0
	.8byte	.LFE0-.LFB0
	.uleb128 0x1
	.byte	0x9c
	.4byte	0xd7
	.uleb128 0x3
	.string	"a"
	.byte	0x1
	.byte	0x9
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -4
	.uleb128 0x3
	.string	"b"
	.byte	0x1
	.byte	0xa
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -8
	.uleb128 0x3
	.string	"and"
	.byte	0x1
	.byte	0xb
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -12
	.uleb128 0x3
	.string	"bic"
	.byte	0x1
	.byte	0xb
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -16
	.uleb128 0x3
	.string	"orr"
	.byte	0x1
	.byte	0xb
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -20
	.uleb128 0x3
	.string	"orn"
	.byte	0x1
	.byte	0xb
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -24
	.uleb128 0x3
	.string	"eor"
	.byte	0x1
	.byte	0xb
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -28
	.uleb128 0x3
	.string	"eon"
	.byte	0x1
	.byte	0xb
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -32
	.uleb128 0x3
	.string	"add"
	.byte	0x1
	.byte	0xc
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -36
	.uleb128 0x3
	.string	"sub"
	.byte	0x1
	.byte	0xc
	.4byte	0xd7
	.uleb128 0x2
	.byte	0x91
	.sleb128 -40
	.byte	0
	.uleb128 0x4
	.byte	0x4
	.byte	0x5
	.string	"int"
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x27
	.uleb128 0x19
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2117
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0x8
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,"",@progbits
	.4byte	0x2c
	.2byte	0x2
	.4byte	.Ldebug_info0
	.byte	0x8
	.byte	0
	.2byte	0
	.2byte	0
	.8byte	.Ltext0
	.8byte	.Letext0-.Ltext0
	.8byte	0
	.8byte	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF0:
	.string	"GNU C11 7.2.1 20170915 (Red Hat Cross 7.2.1-1) -mlittle-endian -mabi=lp64 -g -O0"
.LASF1:
	.string	"program.c"
.LASF3:
	.string	"main"
.LASF2:
	.string	"/home/ulises/Project/chisel/protoflex-chisel/src/test/scala"
	.ident	"GCC: (GNU) 7.2.1 20170915 (Red Hat Cross 7.2.1-1)"
	.section	.note.GNU-stack,"",@progbits
