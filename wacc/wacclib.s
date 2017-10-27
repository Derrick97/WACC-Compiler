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
	.global	wacc_println_bool
	.syntax unified
	.arm
	.type	wacc_println_bool, %function
wacc_println_bool:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	bl	wacc_print_bool
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	wacc_println_bool, .-wacc_println_bool
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
	.align	2
	.global	wacc_println_char
	.syntax unified
	.arm
	.type	wacc_println_char, %function
wacc_println_char:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	bl	wacc_print_char
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	wacc_println_char, .-wacc_println_char
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
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r1, [fp, #-8]
	ldr	r0, .L29
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L30:
	.align	2
.L29:
	.word	.LC4
	.size	wacc_print_string, .-wacc_print_string
	.align	2
	.global	wacc_println_string
	.syntax unified
	.arm
	.type	wacc_println_string, %function
wacc_println_string:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r1, [fp, #-8]
	ldr	r0, .L33
	bl	printf
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L34:
	.align	2
.L33:
	.word	.LC4
	.size	wacc_println_string, .-wacc_println_string
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
	ldr	r0, .L37
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L38:
	.align	2
.L37:
	.word	.LC1
	.size	wacc_print_int, .-wacc_print_int
	.align	2
	.global	println_int
	.syntax unified
	.arm
	.type	println_int, %function
println_int:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	str	r0, [fp, #-8]
	ldr	r0, [fp, #-8]
	bl	wacc_print_int
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	println_int, .-println_int
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
	ldr	r0, .L43
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L44:
	.align	2
.L43:
	.word	.LC5
	.size	print_pair, .-print_pair
	.align	2
	.global	println_pair
	.syntax unified
	.arm
	.type	println_pair, %function
println_pair:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	bl	print_pair
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	println_pair, .-println_pair
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
	ldr	r0, .L49
	bl	printf
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
.L50:
	.align	2
.L49:
	.word	.LC5
	.size	wacc_print_array, .-wacc_print_array
	.align	2
	.global	wacc_println_array
	.syntax unified
	.arm
	.type	wacc_println_array, %function
wacc_println_array:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{fp, lr}
	add	fp, sp, #4
	sub	sp, sp, #8
	mov	r3, r0
	strb	r3, [fp, #-5]
	ldrb	r3, [fp, #-5]	@ zero_extendqisi2
	mov	r0, r3
	bl	wacc_print_array
	mov	r0, #10
	bl	putchar
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #4
	@ sp needed
	pop	{fp, pc}
	.size	wacc_println_array, .-wacc_println_array
	.ident	"GCC: (Ubuntu/Linaro 5.4.0-6ubuntu1~16.04.4) 5.4.0 20160609"
	.section	.note.GNU-stack,"",%progbits
