/*
 * Author: Colin Parker
 */

#include <sys/types.h>
#include <i386/vm_param.h>
#include <i386at/mb_parse.h>
#include <mach/machine/multiboot.h>

void *multiboot2_get_tag(const void *mbi, int tag_type)
{
    const struct multiboot2_start_tag *mb_start;
    const struct multiboot2_generic_tag *mb_search;

    mb_start = mbi;
    mb_search = mbi + sizeof(struct multiboot2_start_tag);

    while ((void *)mb_search < mbi + mb_start->total_size)
    {
	/*if (mb_search->type == tag_type)
		return((void *)mb_search);*/
	if (mb_search->type == MULTIBOOT2_TAG_FRAMEBUFFER)
		spam_framebuffer((struct multiboot2_framebuffer_tag *)mb_search);
	else if (mb_search->type == 0)
		return(NULL);
	mb_search = (void *)mb_search + mb_search->size;
	mb_search = (void *)(((uint32_t)mb_search + 7) & 0xFFFFFFF8);
    }
    
    return(NULL);
}

void spam_framebuffer(struct multiboot2_framebuffer_tag *framebuf_info)
{
	volatile int dontrun = 1;
	volatile int counter = 0;
	while(dontrun)
	{
		counter++;
	}
	
	uint32_t seed = 39;
	uint32_t val = 39;
	uint32_t a;
	uint32_t count;
	uint32_t *framebuffer_base;
	
	framebuffer_base = (uint32_t *)phystokv(framebuf_info->framebuffer_addr);
	count = framebuf_info->framebuffer_pitch*framebuf_info->framebuffer_height/4;
	
	while(1 == 1)
	{
		for(a = 0; a < count; a++)
		{
			framebuffer_base[a] = val;
			val *= seed;
		}
	}
}

void *multiboot2_get_tag_after(const void *mbi, int tag_type, const void *old_tag)
{
    const struct multiboot2_start_tag *mb_start;
    const struct multiboot2_generic_tag *mb_search;

    /* If the old_tag before the first tag just start from the beginning */
    if (old_tag == NULL || old_tag < mbi+sizeof(struct multiboot2_start_tag))
    {
	return(multiboot2_get_tag(mbi, tag_type));
    }
	
    mb_start = mbi;
    mb_search = old_tag;
    
    if ((void *)mb_search < mbi + mb_start->total_size)
    {
	mb_search = (void *)mb_search + mb_search->size;
	mb_search = (void *)(((uint32_t)mb_search + 7) & 0xFFFFFFF8);
    }
    while ((void *)mb_search < mbi + mb_start->total_size)
    {
	if (mb_search->type == tag_type)
		return((void *)mb_search);
	else if (mb_search->type == 0)
		return(NULL);
	mb_search = (void *)mb_search + mb_search->size;
	mb_search = (void *)(((uint32_t)mb_search + 7) & 0xFFFFFFF8);
    }
    
    return(NULL);
}