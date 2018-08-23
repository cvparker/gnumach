
#include <mach/machine/asm.h>

#include <i386/i386asm.h>
#define __MULTIBOOT2__

	/*
	 * This section will be put first into .text.  See also	i386/ldscript.
	 */
	.section .text.start,"ax"
	
	/* We should never be entered this way.  */
	.globl	start,_start
start:
_start:
	jmp	boot_entry

	/* MultiBoot header - see multiboot.h.  */
	
	P2ALIGN(3)
boot_hdr:

#ifdef __MULTIBOOT2__

#define MULTIBOOT2_MAGIC		0xE85250D6
#define MULTIBOOT2_ARCH_I386		0x00000000
#define MULTIBOOT2_OPTIONAL		0x00010000
#define MULTIBOOT2_TAG_INFO_REQ 	1
#define MULTIBOOT2_TAG_CONSOLE_FLAGS	4
#define MULTIBOOT2_TAG_FRAME_BUF	5
#define MULTIBOOT2_TAG_MODULE_ALIGN	6
#define MULTIBOOT2_EFI_SERVICES		7

#define MULTIBOOT2_INFO_REQ_CMD_LINE	1
#define MULTIBOOT2_INFO_REQ_MODS	3
#define MULTIBOOT2_INFO_REQ_MEM_MAP	6
#define MULTIBOOT2_INFO_REQ_FRAME_BUF	8
#define MULTIBOOT2_INFO_REQ_ELF_SYM	9

#define MULTIBOOT2_CONSOLE_FLAGS_EGA	(1 << 1)

	/* Multiboot2 Header Common Part */
	
	.long MULTIBOOT2_MAGIC
	.long MULTIBOOT2_ARCH_I386
	.long (boot_entry-boot_hdr)
	.long -(MULTIBOOT2_MAGIC + MULTIBOOT2_ARCH_I386 + (boot_entry-boot_hdr))
	
	/* Tags */

	/* Info Request */
	P2ALIGN(3)
info_req_start:
	.long MULTIBOOT2_TAG_INFO_REQ
	.long (info_req_end-info_req_start)
	.long MULTIBOOT2_INFO_REQ_CMD_LINE
	.long MULTIBOOT2_INFO_REQ_MODS
	.long MULTIBOOT2_INFO_REQ_MEM_MAP
	.long MULTIBOOT2_INFO_REQ_FRAME_BUF
	.long MULTIBOOT2_INFO_REQ_ELF_SYM
info_req_end:

	/* Console Flags */
	P2ALIGN(3)
console_flags_start:
	.long MULTIBOOT2_TAG_CONSOLE_FLAGS
	.long (console_flags_end-console_flags_start)
	.long MULTIBOOT2_CONSOLE_FLAGS_EGA
console_flags_end:

	/* Frame Buffer */
	/*P2ALIGN(3)
frame_buffer_start:
	.long MULTIBOOT2_TAG_FRAME_BUF
	.long (frame_buffer_end-frame_buffer_start)
	.long 80
	.long 24
	.long 0
frame_buffer_end:*/

	/* Module Align */
	P2ALIGN(3)
module_align_start:
	.long MULTIBOOT2_TAG_MODULE_ALIGN
	.long (module_align_end-module_align_start)
module_align_end:

	/* EFI Services Tag */
	/*P2ALIGN(3)
efi_services_start:
	.long MULTIBOOT2_EFI_SERVICES
	.long (efi_services_end-efi_services_start)
efi_services_end:*/

	/* Terminating empty tag */
	P2ALIGN(3)
	.long 0
	.long 8
#else
#define	MULTIBOOT_MAGIC		0x1BADB002
#ifdef __ELF__
#define MULTIBOOT_FLAGS		0x00000003
#else  /* __ELF__ */
#define MULTIBOOT_FLAGS		0x00010003
#endif /* __ELF__ */


	.long	MULTIBOOT_MAGIC
	.long	MULTIBOOT_FLAGS
		/*
		 * The next item here is the checksum.
		 * XX this works OK until we need at least the 30th bit.
		 */
	.long	- (MULTIBOOT_MAGIC+MULTIBOOT_FLAGS)
#ifndef __ELF__	/* a.out kludge */
	.long	boot_hdr	/* header_addr */
	.long	_start		/* load_addr */
	.long	_edata		/* load_end_addr */
	.long	_end		/* bss_end_addr */
	.long	boot_entry	/* entry */
#endif /* __ELF__ */
#endif /* __MULTIBOOT2__ */
boot_entry:
	/* use segmentation to offset ourself. */
	lgdt	boot_gdt_descr - KERNELBASE
	ljmp	$8,$0f
0:
	movw	$0,%ax
	movw	%ax,%ds
	movw	%ax,%es
	movw	%ax,%fs
	movw	%ax,%gs
	movw	$16,%ax
	movw	%ax,%ds
	movw	%ax,%es
	movw	%ax,%ss

	/* Switch to our own interrupt stack.  */
	movl	$_intstack+INTSTACK_SIZE,%esp

	/* Reset EFLAGS to a known state.  */
	pushl	$0
	popf

	/* Clear uninitialized data.  */
	lea     _edata,%edi
	lea     _end,%ecx
	subl    %edi,%ecx
	xorl    %eax,%eax
	rep
	stosb

	/* Push the boot_info pointer to be the second argument.  */
	pushl	%ebx

	/* Fix ifunc entries */
	movl    $__rel_iplt_start,%esi
	movl    $__rel_iplt_end,%edi
iplt_cont:
	cmpl    %edi,%esi
	jae     iplt_done
	movl    (%esi),%ebx	/* r_offset */
	movb    4(%esi),%al	/* info */
	cmpb    $42,%al		/* IRELATIVE */
	jnz     iplt_next
	call    *(%ebx)		/* call ifunc */
	movl    %eax,(%ebx)	/* fixed address */
iplt_next:
	addl    $8,%esi
	jmp     iplt_cont
iplt_done:

	/* Jump into C code.  */
	call	EXT(c_boot_entry)

	.comm	_intstack,INTSTACK_SIZE

.align 16
	.word 0
boot_gdt_descr:
	.word 3*8+7
	.long boot_gdt - KERNELBASE
.align 16
boot_gdt:
	/* 0 */
	.quad 0
	/* boot CS = 8 */
	.word 0xffff
	.word (-KERNELBASE) & 0xffff
	.byte ((-KERNELBASE) >> 16) & 0xff
	.byte 0x9a
	.byte 0xcf
	.byte ((-KERNELBASE) >> 24) & 0xff
	/* boot DS = 8 */
	.word 0xffff
	.word (-KERNELBASE) & 0xffff
	.byte ((-KERNELBASE) >> 16) & 0xff
	.byte 0x92
	.byte 0xcf
	.byte ((-KERNELBASE) >> 24) & 0xff

