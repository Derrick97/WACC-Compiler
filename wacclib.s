	.cpu arm1176jzf-s
	.fpu softvfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"wacclib.c"
	.text
	.align	2
	.global	wacc_chr
	.syntax unified
	.arm
	.type	wacc_chr, %function
wacc_chr:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	uxtb	r3, r3
	mov	r0, r3
	sub	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	wacc_chr, .-wacc_chr
	.align	2
	.global	wacc_ord
	.syntax unified
	.arm
	.type	wacc_ord, %function
wacc_ord:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	sub	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	wacc_ord, .-wacc_ord
	.section	.rodata
	.align	2
.LC0:
	.ascii	"%c\000"
	.text
	.align	2
	.global	wacc_read_char
	.syntax unified
	.arm
	.type	wacc_read_char, %function
wacc_read_char:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	ldr	r3, .L8
	ldr	r3, [r3]
	str	r3, [fp, #-8]
	sub	r3, fp, #9
	mov	r1, r3
	ldr	r0, .L8+4
	bl	__isoc99_scanf
	ldrb	r3, [fp, #-9]	@ zero_extendqisi2
	mov	r0, r3
	ldr	r3, .L8
	ldr	r2, [fp, #-8]
	ldr	r3, [r3]
	cmp	r2, r3
	beq	.L7
	bl	__stack_chk_fail
.L7:
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L9:
	.align	2
.L8:
	.word	__stack_chk_guard
	.word	.LC0
	.size	wacc_read_char, .-wacc_read_char
	.section	.rodata
	.align	2
.LC1:
	.ascii	"%d\000"
	.text
	.align	2
	.global	wacc_read_int
	.syntax unified
	.arm
	.type	wacc_read_int, %function
wacc_read_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	ldr	r3, .L13
	ldr	r3, [r3]
	str	r3, [fp, #-8]
	sub	r3, fp, #12
	mov	r1, r3
	ldr	r0, .L13+4
	bl	__isoc99_scanf
	ldr	r3, [fp, #-12]
	mov	r0, r3
	ldr	r3, .L13
	ldr	r2, [fp, #-8]
	ldr	r3, [r3]
	cmp	r2, r3
	beq	.L12
	bl	__stack_chk_fail
.L12:
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L14:
	.align	2
.L13:
	.word	__stack_chk_guard
	.word	.LC1
	.size	wacc_read_int, .-wacc_read_int
	.section	.rodata
	.align	2
.LC2:
	.ascii	"true\000"
	.align	2
.LC3:
	.ascii	"false\000"
	.text
	.align	2
	.global	wacc_print_bool
	.syntax unified
	.arm
	.type	wacc_print_bool, %function
wacc_print_bool:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	cmp	r3, #0
	beq	.L16
	ldr	r0, .L19
	bl	printf
	b	.L17
.L16:
	ldr	r0, .L19+4
	bl	printf
.L17:
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L20:
	.align	2
.L19:
	.word	.LC2
	.word	.LC3
	.size	wacc_print_bool, .-wacc_print_bool
	.align	2
	.global	wacc_println
	.syntax unified
	.arm
	.type	wacc_println, %function
wacc_println:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	pop	{fp, pc}
	.size	wacc_println, .-wacc_println
	.align	2
	.global	wacc_print_char
	.syntax unified
	.arm
	.type	wacc_print_char, %function
wacc_print_char:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	wacc_print_char, .-wacc_print_char
	.section	.rodata
	.align	2
.LC4:
	.ascii	"%s\000"
	.text
	.align	2
	.global	wacc_print_string
	.syntax unified
	.arm
	.type	wacc_print_string, %function
wacc_print_string:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r1, [fp, #-8]
	ldr	r0, .L27
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L28:
	.align	2
.L27:
	.word	.LC4
	.size	wacc_print_string, .-wacc_print_string
	.align	2
	.global	wacc_print_int
	.syntax unified
	.arm
	.type	wacc_print_int, %function
wacc_print_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r1, [fp, #-8]
	ldr	r0, .L31
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L32:
	.align	2
.L31:
	.word	.LC1
	.size	wacc_print_int, .-wacc_print_int
	.section	.rodata
	.align	2
.LC5:
	.ascii	"0x%x\000"
	.text
	.align	2
	.global	print_pair
	.syntax unified
	.arm
	.type	print_pair, %function
print_pair:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r1, r3
	ldr	r0, .L35
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L36:
	.align	2
.L35:
	.word	.LC5
	.size	print_pair, .-print_pair
	.align	2
	.global	wacc_len
	.syntax unified
	.arm
	.type	wacc_len, %function
wacc_len:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #12
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	ldr	r3, [r3]
	mov	r0, r3
	sub	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	wacc_len, .-wacc_len
	.align	2
	.global	wacc_print_array
	.syntax unified
	.arm
	.type	wacc_print_array, %function
wacc_print_array:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r1, r3
	ldr	r0, .L41
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L42:
	.align	2
.L41:
	.word	.LC5
	.size	wacc_print_array, .-wacc_print_array
	.align	2
	.global	wacc_exit
	.syntax unified
	.arm
	.type	wacc_exit, %function
wacc_exit:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	bl	exit
	.size	wacc_exit, .-wacc_exit
	.section	.rodata
	.align	2
.LC6:
	.ascii	"OverflowError: the result is too small/large to sto"
	.ascii	"re in a 4-byte signed-integer.\000"
	.text
	.align	2
	.global	wacc_throw_overflow_error
	.syntax unified
	.arm
	.type	wacc_throw_overflow_error, %function
wacc_throw_overflow_error:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	ldr	r0, .L45
	bl	puts
	mov	r0, #255
	bl	exit
.L46:
	.align	2
.L45:
	.word	.LC6
	.size	wacc_throw_overflow_error, .-wacc_throw_overflow_error
	.ident	"GCC: (Ubuntu/Linaro 5.4.0-6ubuntu1~16.04.4) 5.4.0 20160609"
	.section	.note.GNU-stack,"",%progbits
