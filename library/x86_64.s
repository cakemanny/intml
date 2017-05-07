.text
.global ml__write
ml__write:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rsi, -32(%rbp)
	leaq	ml__write$(%rip), %rax
	movq	%rax, -40(%rbp)
	movq	$8, %rdi
	callq	_malloc
	movq	%rax, %r10          # r10 = pclosure
	movq	-32(%rbp), %rax		# var fd local word 1
	movq	%rax, (%r10)        # store fd in closure
	movq	-40(%rbp), %rax		# var write$ local word 1 - fn addr
	movq	%r10, %rdi		    # var write$ local word 2 - pnext
	addq	$48, %rsp
	popq	%rbp
	retq
ml__write$:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, %r10		    # addr closure
	movq	(%r10), %rdi		# var fd closure word 1 - arg1
	xchgq	%rdx, %rsi		# strings are len,char* but for write arg2 = char*, arg3 = len
	callq   _write		# result goes in rax
	popq	%rbp
	retq
