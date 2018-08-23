/*
 * Author: Colin Parker
 */

#ifndef _MACH_I386_MB_PARSE_H_
#define _MACH_I386_MB_PARSE_H_

#include <mach/machine/multiboot.h>

void *multiboot2_get_tag(const void *mbi, int tag_type);

void *multiboot2_get_tag_after(const void *mbi, int tag_type, const void *old_tag);

void spam_framebuffer(struct multiboot2_framebuffer_tag *framebuf_info);

#endif /* _MACH_I386_MB_PARSE_H_ */