/*
 * Mach Operating System
 * Copyright (c) 1991,1990,1989 Carnegie Mellon University
 * All Rights Reserved.
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation is hereby granted, provided that both the copyright
 * notice and this permission notice appear in all copies of the
 * software, derivative works or modified versions, and any portions
 * thereof, and that both notices appear in supporting documentation.
 *
 * CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
 * CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
 * ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
 *
 * Carnegie Mellon requests users of this software to return to
 *
 *  Software Distribution Coordinator  or  Software.Distribution@CS.CMU.EDU
 *  School of Computer Science
 *  Carnegie Mellon University
 *  Pittsburgh PA 15213-3890
 *
 * any improvements or extensions that they make and grant Carnegie Mellon
 * the rights to redistribute these changes.
 */
/*
 *	Olivetti Mach Console driver v0.0
 *	Copyright Ing. C. Olivetti & C. S.p.A. 1988, 1989
 *	All rights reserved.
 *
 */
/*
  Copyright 1988, 1989 by Olivetti Advanced Technology Center, Inc.,
Cupertino, California.

		All Rights Reserved

  Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appears in all
copies and that both the copyright notice and this permission notice
appear in supporting documentation, and that the name of Olivetti
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.

  OLIVETTI DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL OLIVETTI BE LIABLE FOR ANY SPECIAL, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN ACTION OF CONTRACT,
NEGLIGENCE, OR OTHER TORTIOUS ACTION, ARISING OUR OF OR IN CONNECTION
WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/*
  Copyright 1988, 1989 by Intel Corporation, Santa Clara, California.

		All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appears in all
copies and that both the copyright notice and this permission notice
appear in supporting documentation, and that the name of Intel
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.

INTEL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL INTEL BE LIABLE FOR ANY SPECIAL, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN ACTION OF CONTRACT,
NEGLIGENCE, OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/* Modified for framebuffer by Colin Parker */

/* $ Header:  $ */

#include <sys/types.h>
#include <kern/debug.h>
#include <kern/mach_clock.h>
#include <kern/printf.h>
#include <device/conf.h>
#include <device/tty.h>
#include <device/io_req.h>
#include <device/buf.h>
#include <vm/vm_kern.h>
#include <i386/locore.h>
#include <i386/loose_ends.h>
#include <i386/vm_param.h>
#include <i386/machspl.h>
#include <i386/pio.h>
#include <i386at/cram.h>
#include <i386at/fbcons.h>
#include <i386at/mb_parse.h>
#include <device/cons.h>
#include <util/atoi.h>

struct tty       fb_tty;
extern boolean_t fb_rebootflag;

#if 0
static void charput(), charmvup(), charmvdown(), charclear(), charsetcursor();
static void fb_noopreset(void);

/*
 * These routines define the interface to the device-specific layer.
 * 
 */
void	(*fb_dput)()	= charput;	/* put attributed char */
void	(*fb_dmvup)()	= charmvup;	/* block move up */
void	(*fb_dmvdown)()	= charmvdown;	/* block move down */
void	(*fb_dclear)()	= charclear;	/* block clear */
void	(*fb_dsetcursor)() = charsetcursor;
				/* set cursor position on displayed page */
void	(*fb_dreset)() = fb_noopreset;	/* prepare for reboot */
#endif /* 0 */

/*
 * Globals used for both character-based controllers and bitmap-based
 * controllers.  Default is EGA.
 */

u_char 	*frame_buf_start	= 0;	/* VM start of video RAM or frame buffer */
csrpos_t fb_curpos	= 0;	/* set indirectly by fb_setpos--see kdsoft.h */
short	fb_lines	= 0;
short	fb_cols		= 0;
char	fb_attr		= KA_NORMAL;	/* current attribute */
char	fb_color	= KA_NORMAL;
char	fb_attrflags	= 0;		/* Not reverse, underline, blink */


/*
 * We don't provide any mutex protection for this flag because we know
 * that this module will have been initialized by the time multiple
 * threads are running.
 */
boolean_t fb_initialized 	= FALSE;	/* driver initialized? */

/* Array for processing escape sequences. */
#define	K_MAXESC	32
u_char	fb_esc_seq[K_MAXESC];
u_char	*fb_esc_spt	= (u_char *)0;

/*#include <i386at/fbcons_font.h>*/
#define FB_CHARS_IN_FONT 256
#define FB_FONT_HEIGHT 22
#define FB_FONT_WIDTH 10
#define FB_CURSOR_HEIGHT (24 - FB_FONT_HEIGHT)
uint32_t fb_font[FB_CHARS_IN_FONT][FB_FONT_HEIGHT];

short	fb_fb_width	 = 0;		/* bits in frame buffer scan line */
short	fb_fb_height	 = 0;		/* scan lines in frame buffer*/
short	fb_char_width	 = FB_FONT_WIDTH;/* bit width of 1 char */
short	fb_char_height	 = FB_FONT_HEIGHT;/* bit height of 1 char */
short	fb_chars_in_font = FB_CHARS_IN_FONT;
short	fb_cursor_height = FB_CURSOR_HEIGHT;/* bit height of cursor */
short	fb_bit_depth	 = 0;

/* These initial values are simply guesses. */
uint32_t	fb_char_black	= 0;
uint32_t	fb_char_white	= 0;

short	fb_xstart		= 0;
short	fb_ystart		= 0;

short	fb_fb_byte_width	= 0;		


#ifdef	DEBUG

void
pause(void)
{
	int i;

	for (i = 0; i < 50000; ++i)
		;
}

/*
 * Put a debugging character on the screen.
 * LOC=0 means put it in the bottom right corner, LOC=1 means put it
 * one column to the left, etc.
 */
void
fb_debug_put(
	int	loc,
	char	c)
{
	csrpos_t pos = ONE_PAGE - (loc+1) * ONE_SPACE;

	(*fb_dput)(pos, c, KA_NORMAL);
}
#endif /* DEBUG */


#if 0

extern boolean_t	mouse_in_use;
int			old_kb_mode;

void
cnpollc(boolean_t on)
{
	if (mouse_in_use) {
		if (on) {
		    /* switch into X */
		    old_kb_mode = kb_mode;
		    kb_mode = KB_ASCII;
		    X_kdb_enter();

		    fb_pollc++;
		} else {
		    --fb_pollc;

		    /* switch out of X */
		    X_kdb_exit();
		    kb_mode = old_kb_mode;
		}
	} else {
		if (on) {
		    fb_pollc++;
		} else {
		    --fb_pollc;
		}
	}
}


#endif /* 0 */

/*
 * fbopen:
 *
 *	This opens the console driver and sets up the tty and other
 *	rudimentary stuff including calling the line discipline for
 *	setting up the device independent stuff for a tty driver.
 *
 * input:	device number 'dev', and flag
 *
 * output:	device is opened and setup
 *
 */
int
fbopen(
	dev_t	 dev,
	int	 flag,
	io_req_t ior)
{
	struct 	tty	*tp;
	spl_t	o_pri;

	tp = &fb_tty;
	o_pri = spltty();
	simple_lock(&tp->t_lock);
	if (!(tp->t_state & (TS_ISOPEN|TS_WOPEN))) {
		/* XXX ttychars allocates memory */
		simple_unlock(&tp->t_lock);
		ttychars(tp);
		simple_lock(&tp->t_lock);
		/*
		 *	Special support for boot-time rc scripts, which don't
		 *	stty the console.
		 */
		tp->t_oproc = fbstart;
		tp->t_stop = fbstop;
		tp->t_ospeed = tp->t_ispeed = B9600;
		tp->t_flags = ODDP|EVENP|ECHO|CRMOD|XTABS;
		fbinit();
	}
	tp->t_state |= TS_CARR_ON;
	simple_unlock(&tp->t_lock);
	splx(o_pri);
	return (char_open(dev, tp, flag, ior));
}


/*
 * fbclose:
 *
 *	This function merely executes the device independent code for
 *	closing the line discipline.
 *
 * input:	device number 'dev', and flag
 *
 * output:	device is closed
 *
 */
/*ARGSUSED*/
void
fbclose(dev, flag)
dev_t	dev;
int	flag;
{
	struct	tty	*tp;

	tp = &fb_tty;
	{
	    spl_t s = spltty();
	    simple_lock(&tp->t_lock);
	    ttyclose(tp);
	    simple_unlock(&tp->t_lock);
	    splx(s);
	}

	return;
}


/*
 * fbread:
 *
 *	This function executes the device independent code to read from
 *	the tty.
 *
 * input:	device number 'dev'
 *
 * output:	characters are read from tty clists
 *
 */
/*ARGSUSED*/
int
fbread(dev, uio)
dev_t	dev;
io_req_t uio;
{
	struct	tty	*tp;

	tp = &fb_tty;
	tp->t_state |= TS_CARR_ON;
	return((*linesw[fb_tty.t_line].l_read)(tp, uio));
}


/*
 * fbwrite:
 *
 *	This function does the device independent write action for this
 *	console (tty) driver.
 *
 * input:	device number 'dev'
 *
 * output:	characters are written to tty clists
 *
 */
/*ARGSUSED*/
int
fbwrite(dev, uio)
dev_t	dev;
io_req_t uio;
{
	return((*linesw[fb_tty.t_line].l_write)(&fb_tty, uio));
}

/*
 * Mmap.
 */

/*ARGSUSED*/
int
fbmmap(dev, off, prot)
	dev_t dev;
	vm_offset_t off;
	vm_prot_t prot;
{
	if (off >= (fb_fb_height*fb_fb_byte_width))
		return(-1);

	/* Get page frame number for the page to be mapped. */
	return(i386_btop(frame_buf_start+off));
}

int
fbportdeath(
	dev_t		dev,
	mach_port_t	port)
{
	return (tty_portdeath(&fb_tty, (ipc_port_t)port));
}

/*ARGSUSED*/
io_return_t fbgetstat(
	dev_t		dev,
	int		flavor,
	int *		data,		/* pointer to OUT array */
	natural_t	*count)		/* OUT */
{
	io_return_t	result;

	switch (flavor) {
#if 0
	    case FBGSTATE:
		if (*count < 1)
		    return (D_INVALID_OPERATION);
		*data = fb_state;
		*count = 1;
		result = D_SUCCESS;
		break;
	    case FBGKBENT:
		result = fbgetkbent((struct kbentry *)data);
		*count = sizeof(struct kbentry)/sizeof(int);
		break;
#endif
	    default:
		result = tty_get_status(&fb_tty, flavor, data, count);
		break;
	}
	return (result);
}

/*ARGSUSED*/
io_return_t fbsetstat(
	dev_t		dev,
	int		flavor,
	int *		data,
	natural_t	count)
{
	io_return_t	result;

	switch (flavor) {
#if 0
	    case FBSKBENT:
		if (count < sizeof(struct kbentry)/sizeof(int)) {
		    return (D_INVALID_OPERATION);
		}
		result = fbsetkbent((struct kbentry *)data, 0);
		break;
#endif
	    case FBSETBELL:
		if (count < 1)
		    return (D_INVALID_OPERATION);
		result = fbsetbell(*data, 0);
		break;

	    default:
		result = tty_set_status(&fb_tty, flavor, data, count);
	}
	return (result);
}



/*
 * fbsetbell:
 *
 *	Turn the bell on or off.  Returns error code, if given bogus
 *	on/off value.
 */
int
fbsetbell(
	int	val,				/* on or off */
	int	flags)				/* flags set for console */
{
	int err = 0;

	if (val == FB_BELLON)
		fb_bellon();
	else if (val == FB_BELLOFF)
		fb_belloff(NULL);
	else
		err = D_INVALID_OPERATION;

	return(err);
}

static boolean_t fb_bellstate = FALSE;

void fb_belloff(void *unused)
{
}

void fb_bellon(void)
{
}

#if 0

/*
 * fbgetkbent:
 *
 *	Get entry from key mapping table.  Returns error code, if any.
 */
int
fbgetkbent(struct kbentry *kbent)
{
	u_char *cp;
	spl_t o_pri = SPLKD();		/* probably superfluous */

	cp = &key_map[kbent->kb_index][CHARIDX(kbent->kb_state)];
	kbent->kb_value[0] = *cp++;
	kbent->kb_value[1] = *cp++;
	kbent->kb_value[2] = *cp;
	(void)splx(o_pri);
	return(0);
}


/*
 * fbsetkbent:
 *
 *	Set entry in key mapping table.  Return error code, if any.
 */
int
fbsetkbent(
	struct kbentry 	*kbent,
	int		flags)				/* flags set for console */
{
	u_char *cp;
	spl_t o_pri;

	o_pri = SPLKD();
	cp = &key_map[kbent->kb_index][CHARIDX(kbent->kb_state)];
	*cp++ = kbent->kb_value[0];
	*cp++ = kbent->kb_value[1];
	*cp = kbent->kb_value[2];
	(void)splx(o_pri);
	return(0);
}

/*
 * fbintr:
 *
 *	This function is the interrupt code for the driver.  Since this is
 *	a special tty (console), interrupts are only for input, so we read in
 *	the character.  If in ascii mode, we then do the mapping translation
 *	from the keyboard switch table and place the characters on the tty's
 *	input switch table.  If in event mode, we create and queue a fb_event.
 *
 * input:	interrupt vector 'vec'
 *
 * output:	character or sequence is placed on appropriate queue
 *
 */
/*ARGSUSED*/
void
fbintr(int vec)
{
	struct	tty	*tp;
	unsigned char	c;
	unsigned char	scancode;
	unsigned int	char_idx;
	boolean_t	up = FALSE;		/* key-up event */

	if (fb_pollc)
	    return;				/* kdb polling kbd */

	if (!fb_initialized)
		return;

	tp = &fb_tty;
#ifdef	old
	while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
		;	/* this should never loop */
#else	/* old */
	{
		/*
		 * Allow for keyboards that raise interrupt before
		 * the character gets to the buffer.  But don't wait
		 * forever if grabbing the character by polling leaves
		 * the interrupt on but buffer empty.
		 */
		/*
		 * Micronics VLB motherboard with 486DX2 can report keyboard
		 * interrupt before K_STATUS register indicates that the
		 * output buffer is full.  Moreover, the bus won't settle w
		 * while we poll K_STATUS at speed.  Temporary fix is to break
		 * out after safety runs out and pick up keyboard event.  This
		 * should be fixed eventually by putting a 1us timout between
		 * inb's to K_STATUS and fix the pic initialization order to
		 * avoid bootup keyboard wedging (ie make kd a real device)
		 */
		int safety = 1000;
		while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
			if (!safety--) break;  /* XXX */
	}
#endif	/* old */
	/*
	 * We may have seen a mouse event.
	 */
	if ((inb(K_STATUS) & 0x20) == 0x20) {
		if (mouse_in_use) {
			mouse_handle_byte((u_char)inb(K_RDWR));
			return;
		} else {
			printf("M%xI", inb(K_RDWR));
			return;
		}
	}

	scancode = inb(K_RDWR);
	if (scancode == K_EXTEND && kb_mode != KB_EVENT) {
		fb_extended = TRUE;
		goto done;
	} else if (scancode == K_RESEND) {
		fb_resend();
		goto done;
	} else if (scancode == K_ACKSC) {
		fb_handle_ack();
		goto done;
	} else if (fb_kbd_mouse && fb_kbd_magic(scancode)) {
		goto done;
	} else if (fbcheckmagic(scancode)) {
		goto done;
	} else if (kb_mode == KB_EVENT) {
		fb_enqsc(scancode);
		goto done;
	} /* else... */

	if (scancode & K_UP) {
		up = TRUE;
		scancode &= ~K_UP;
	}
	if (scancode < NUMKEYS) {
		/* Lookup in map, then process. */
		char_idx = fbstate2idx(fb_state, fb_extended);
		c = key_map[scancode][char_idx];
		if (c == K_SCAN) {
			c = key_map[scancode][++char_idx];
			set_fb_state(do_modifier(fb_state, c, up));
		} else if (!up) {
			/* regular key-down */
			unsigned int max; /* max index for char sequence */

			max = char_idx + NUMOUTPUT;
			char_idx++;
			if (!fb_extended) {
				if (fb_state&KS_CLKED) {
 					if (fb_isupper(c)) {
						c += ('a' - 'A');
						max = char_idx;
					}
					else if (fb_islower(c)) {
						c -= ('a' - 'A');
						max = char_idx;
					}
				}
				/*
				 * Notice that even if the keypad is remapped,
				 * NumLock only effects the keys that are
				 * physically part of the keypad.  Is this
				 * The Right Thing?
				 */
				if ((fb_state&KS_NLKED) &&
			    	    (((K_HOMESC) <= scancode) &&
			     	    (scancode <= (K_DELSC)))) {
					char_idx = CHARIDX(SHIFT_STATE);
					c = key_map[scancode][char_idx];
					max = char_idx + NUMOUTPUT;
					char_idx++;
				}
			}

			/*
			 * here's where we actually put the char (or
			 * char sequence, for function keys) onto the
			 * input queue.
			 */
			for ( ; (c != K_DONE) && (char_idx <= max);
			     c = key_map[scancode][char_idx++]) {
				(*linesw[tp->t_line].l_rint)(c, tp);
			}
			fb_extended = FALSE;
		}
	}

 done:
	return;
}

/*
 * fb_handle_ack:
 *
 *	For pending commands, complete the command.  For data bytes,
 *	drop the ack on the floor.
 */
void
fb_handle_ack(void)
{
	switch (fb_ack) {
	case SET_LEDS:
		fb_setleds2();
		fb_ack = DATA_ACK;
		break;
	case DATA_ACK:
		fb_ack = NOT_WAITING;
		break;
	case NOT_WAITING:
		printf("unexpected ACK from keyboard\n");
		break;
	default:
		panic("bogus fb_ack\n");
		break;
	}
}

/*
 * fb_resend:
 *
 *	Resend a missed keyboard command or data byte.
 */
void
fb_resend(void)
{
	if (fb_ack == NOT_WAITING)
		printf("unexpected RESEND from keyboard\n");
	else
		fb_senddata(last_sent);
}


/*
 * do_modifier:
 *
 *	Change keyboard state according to which modifier key and
 *	whether it went down or up.
 *
 * input:	the current state, the key, and the key's direction.
 *		The key can be any key, not just a modifier key.
 *
 * output:	the new state
 */
int
do_modifier(
	int		state,
	Scancode	c,
	boolean_t	up)
{
	switch (c) {
	case (K_ALTSC):
		if (up)
			state &= ~KS_ALTED;
		else
			state |= KS_ALTED;
		fb_extended = FALSE;
		break;
#ifndef	ORC
	case (K_CLCKSC):
#endif	/* ORC */
	case (K_CTLSC):
		if (up)
			state &= ~KS_CTLED;
		else
			state |= KS_CTLED;
		fb_extended = FALSE;
		break;
#ifdef	ORC
	case (K_CLCKSC):
		if (!up)
			state ^= KS_CLKED;
		break;
#endif	/* ORC */
	case (K_NLCKSC):
		if (!up)
			state ^= KS_NLKED;
		break;
	case (K_LSHSC):
	case (K_RSHSC):
		if (up)
			state &= ~KS_SHIFTED;
		else
			state |= KS_SHIFTED;
		fb_extended = FALSE;
		break;
	}

	return(state);
}


/*
 * fbcheckmagic:
 *
 *	Check for magic keystrokes for invoking the debugger or
 *	rebooting or ...
 *
 * input:	an unprocessed scancode
 *
 * output:	TRUE if a magic key combination was recognized and
 *		processed.  FALSE otherwise.
 *
 * side effects:
 *		various actions possible, depending on which keys are
 *		pressed.  If the debugger is called, steps are taken
 *		to ensure that the system doesn't think the magic keys
 *		are still held down.
 */
boolean_t
fbcheckmagic(Scancode scancode)
{
	static int magic_state = KS_NORMAL; /* like fb_state */
	boolean_t up = FALSE;

	if (scancode == 0x46)		/* scroll lock */
/*	if (scancode == 0x52)		** insert key */
	{
		fb_kbd_mouse = !fb_kbd_mouse;
		fb_kbd_magic_button = 0;
		return(TRUE);
	}
	if (scancode & K_UP) {
		up = TRUE;
		scancode &= ~K_UP;
	}
	magic_state = do_modifier(magic_state, scancode, up);

	if ((magic_state&(KS_CTLED|KS_ALTED)) == (KS_CTLED|KS_ALTED)) {
		switch (scancode) {
#if	MACH_KDB
		case K_dSC:		/*  ctl-alt-d */
			fbb_kintr();	/* invoke debugger */
			/* Returned from debugger, so reset kbd state. */
			(void)SPLKD();
			magic_state = KS_NORMAL;
			if (kb_mode == KB_ASCII)
				fb_state = KS_NORMAL;
				/* setting leds kills kbd */
			else {
				fb_enqsc(K_ALTSC | K_UP);
				fb_enqsc(K_CTLSC | K_UP);
				fb_enqsc(K_dSC | K_UP);
			}
			return(TRUE);
			break;
#endif	/* MACH_KDB */
		case K_DELSC:		/* ctl-alt-del */
			/* if rebootflag is on, reboot the system */
			if (rebootflag)
				fbreboot();
			break;
		}
	}
	return(FALSE);
}


/*
 * fbstate2idx:
 *
 *	Return the value for the 2nd index into key_map that
 *	corresponds to the given state.
 */
unsigned int
fbstate2idx(state, extended)
unsigned int	state;			/* bit vector, not a state index */
boolean_t	extended;
{
	int state_idx = NORM_STATE;

	if ((!extended) && state != KS_NORMAL) {
		if ((state&(KS_SHIFTED|KS_ALTED)) == (KS_SHIFTED|KS_ALTED))
			state_idx = SHIFT_ALT;
                /* CTRL should have higher priority than SHIFT.  That
                   way, CTRL-SHIFT-2 and CTRL-2 produce the same keycode.
                   --Derek Upham 1997/06/25 */
		else if (state&KS_CTLED)
			state_idx = CTRL_STATE;
		else if (state&KS_SHIFTED)
			state_idx = SHIFT_STATE;
		else if (state&KS_ALTED)
			state_idx = ALT_STATE;
	}

	return (CHARIDX(state_idx));
}

#endif /* 0 */

/*
 * fbstart:
 *
 *	This function does the general processing of characters and other
 *	operations for the device driver.  The device independent portion of
 *	the tty driver calls this routine (it's setup in fbinit) with a
 *	given command.  That command is then processed, and control is passed
 *	back to the kernel.
 *
 * input:	tty pointer 'tp', and command to execute 'cmd'
 *
 * output:	command is executed
 *
 * Entered and left at spltty.  Drops priority to spl0 to display character.
 * ASSUMES that it is never called from interrupt-driven code.
 */
void
fbstart(struct tty *tp)
{
	spl_t	o_pri;
	int	ch;

	if (tp->t_state & TS_TTSTOP)
		return;
	for ( ; ; ) {
		tp->t_state &= ~TS_BUSY;
		if (tp->t_state & TS_TTSTOP)
			break;
		if ((tp->t_outq.c_cc <= 0) || (ch = getc(&tp->t_outq)) == -1)
			break;
		/*
		 * Drop priority for long screen updates. ttstart() calls us at
		 * spltty.
		 */
		o_pri = splsoftclock();		/* block timeout */
		fb_putc_esc(ch);
		splx(o_pri);
	}
	if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
		tt_write_wakeup(tp);
	}
}

/*ARGSUSED*/
void
fbstop(
	struct tty 	*tp,
	int		flags)
{
	/*
	 * do nothing - all characters are output by one call to
	 * fbstart.
	 */
}

extern void *boot_info;
/*
 * fbinit:
 *
 *	This code initializes the structures and sets up the port registers
 *	for the console driver.
 *
 *	Each bitmap-based graphics card is likely to require a unique
 *	way to determine the card's presence.  The driver runs through
 *	each "special" card that it knows about and uses the first one
 *	that it finds.  If it doesn't find any, it assumes that an
 *	EGA-like card is installed.
 *
 * input	: None.	Interrupts are assumed to be disabled
 * output	: Driver is initialized
 *
 */
void
fbinit(void)
{
	struct multiboot2_framebuffer_tag *mb_fb;

	if (fb_initialized)
		return;

	fb_esc_spt = fb_esc_seq;
	fb_attr = KA_NORMAL;

	fb_attrflags = 0;
	fb_color = KA_NORMAL;

        mb_fb = multiboot2_get_tag(boot_info, MULTIBOOT2_TAG_FRAMEBUFFER);
        if(mb_fb == NULL || mb_fb->framebuffer_type != 1)
               return;
        frame_buf_start = (u_char *)(mb_fb->framebuffer_addr);
        fb_fb_height = mb_fb->framebuffer_height;
        fb_fb_width = mb_fb->framebuffer_width;
        fb_bit_depth = mb_fb->framebuffer_bpp;
        fb_char_black = 0;
        fb_char_white = (1 << fb_bit_depth) - 1;
        fb_fb_byte_width = (fb_fb_width*fb_bit_depth - 1)/8 + 1;
        fb_lines = fb_fb_height/(fb_char_height+fb_cursor_height);
	fb_ystart = fb_fb_height-fb_lines*(fb_char_height+fb_cursor_height);
        fb_cols = fb_fb_width/fb_char_width;
        fb_xstart = fb_fb_width - fb_cols*fb_char_width;
				   

#if	ENABLE_IMMEDIATE_CONSOLE
	/* Now that we're set up, we no longer need or want the
           immediate console.  */
	{
		extern boolean_t immediate_console_enable;
		immediate_console_enable = FALSE;
	}

	/* The immediate console printed stuff at the bottom of the
	   screen rather than at the cursor position, so that's where
	   we should start.  */
	fb_setpos(ONE_PAGE - ONE_LINE); printf("\n");
#endif /* ENABLE_IMMEDIATE_CONSOLE */

}

/*
 *
 * Function fb_putc_esc():
 *
 * 	This function puts a character on the screen, handling escape
 * 	sequences.
 *
 * input	: character to be displayed (or part of an escape code)
 * output	: character is displayed, or some action is taken
 *
 */
void
fb_putc_esc(u_char c)
{
	if (c == (K_ESC)) {
		if (fb_esc_spt == fb_esc_seq) {
			*(fb_esc_spt++)=(K_ESC);
			*(fb_esc_spt) = '\0';
		} else {
			fb_putc((K_ESC));
			fb_esc_spt = fb_esc_seq;
		}
	} else {
		if (fb_esc_spt - fb_esc_seq) {
			if (fb_esc_spt - fb_esc_seq > K_MAXESC - 1)
				fb_esc_spt = fb_esc_seq;
			else {
				*(fb_esc_spt++) = c;
				*(fb_esc_spt) = '\0';
				fb_parseesc();
			}
		} else {
			fb_putc(c);
		}
	}
}

/*
 *
 * Function fb_putc():
 *
 *	This function simply puts a character on the screen.  It does some
 *	special processing for linefeed, carriage return, backspace and
 *	the bell.
 *
 * input	: character to be displayed
 * output	: character is displayed, or some action is taken
 *
 */
int fb_sit_for_0 = 1;

void
fb_putc(u_char ch)
{
	if ((!ch) && fb_sit_for_0)
		return;

	switch (ch) {
	case ((K_LF)):
		fb_down();
		break;
	case ((K_CR)):
		fb_cr();
		break;
	case ((K_BS)):
		fb_left();
		break;
	case ((K_HT)):
		fb_tab();
		break;
	case ((K_BEL)):
		/*
		 * Similar problem to K_BS here (behavior might depend
		 * on tty setting).  Also check LF and CR.
		 */
	        if (!fb_bellstate)
		  {
		    fb_bellon();
		    timeout(fb_belloff, 0, hz/8 );
		    fb_bellstate = TRUE;
		  }
		break;
	default:
		fbput(fb_curpos, ch, fb_attr);
		fb_right();
		break;
	}
	return;
}

/*
 * fb_setpos:
 *
 *	This function sets the software and hardware cursor position
 *	on the screen, using device-specific code to actually move and
 *	display the cursor.
 *
 * input	: position on (or off) screen to move the cursor to
 * output	: cursor position is updated, screen has been scrolled
 *		  if necessary to bring cursor position back onto
 *		  screen.
 *
 */
void
fb_setpos(csrpos_t newpos)
{
	if (newpos > ONE_PAGE) {
		fb_scrollup();
		newpos = BOTTOM_LINE;
	}
	if (newpos < 0) {
		fb_scrolldn();
		newpos = 0;
	}

	fbsetcursor(newpos);
}


/*
 * fb_scrollup:
 *
 *	This function scrolls the screen up one line using a DMA memory
 *	copy.
 *
 * input	: None
 * output	: lines on screen appear to be shifted up one line
 *
 */
void
fb_scrollup(void)
{
	csrpos_t to;
	csrpos_t from;
	int	count;

	/* scroll up */
	to = 0;
	from = ONE_LINE;
	count = BOTTOM_LINE;
	fbmvup(from, to, count);

	/* clear bottom line */
	to = BOTTOM_LINE;
	count = ONE_LINE;
	fbclear(to, count, fb_attr);
	return;
}

/*
 * fb_scrolldn:
 *
 *	Scrolls the characters on the screen down one line.
 *
 * input	: None
 * output	: Lines on screen appear to be moved down one line
 *
 */
void
fb_scrolldn(void)
{
	csrpos_t to;
	csrpos_t from;
	int	count;

	/* move down */
	to 	= ONE_PAGE - 1;
	from 	= BOTTOM_LINE - 1;
	count 	= BOTTOM_LINE;
	fbmvdown(from, to, count);

	/* clear top line */
	to	= 0;
	count	= ONE_LINE;
	fbclear(to, count, fb_attr);
	return;

}


/*
 * fb_parseesc:
 *
 *	This routine begins the parsing of an escape sequence.  It uses the
 *	escape sequence array and the escape spot pointer to handle
 *	asynchronous parsing of escape sequences.
 *
 * input	: String of characters prepended by an escape
 * output	: Appropriate actions are taken depending on the string as
 *		  defined by the ansi terminal specification
 *
 */
void
fb_parseesc(void)
{
	u_char	*escp;

	escp = fb_esc_seq + 1;		/* point to char following ESC */
	switch(*(escp)) {
	case 'c':
		fb_cls();
		fb_home();
		fb_esc_spt = fb_esc_seq;  /* reset spot in ESC sequence */
		break;
	case '[':
		escp++;
		fb_parserest(escp);
		break;
	case '\0':
		break;			/* not enough info yet	*/
	default:
		fb_putc(*escp);
		fb_esc_spt = fb_esc_seq;	/* inv sequence char, reset */
		break;
	}
	return;

}

/* fb_update_fb_attr:
 *
 *	Updates fb_attr according to fb_attrflags and fb_color.
 * This code has its origin from console.c and selection.h in
 * linux 2.2 drivers/char/.
 * Modified for GNU Mach by Marcus Brinkmann.
 */

#define reverse_video_char(a)       (((a) & 0x88) | ((((a) >> 4) | ((a) << 4)) & 0x77))
void
fb_update_fb_attr(void)
{
	fb_attr = fb_color;
	if (fb_attrflags & KAX_UNDERLINE)
		fb_attr = (fb_attr & 0xf0) | KAX_COL_UNDERLINE;
	else if (fb_attrflags & KAX_DIM)
		fb_attr = (fb_attr & 0xf0) | KAX_COL_DIM;
	if (fb_attrflags & KAX_REVERSE)
		fb_attr = reverse_video_char(fb_attr);
	if (fb_attrflags & KAX_BLINK)
		fb_attr ^= 0x80;
	if (fb_attrflags & KAX_BOLD)
		fb_attr ^= 0x08;
}	

/* color_table added by Julio Merino to take proper color order.
 * I get this code from Linux 2.2 source code in file: 
 * linux/drivers/char/console.c
 */
unsigned char fb_color_table[] = { 0, 4, 2, 6, 1, 5, 3, 7,
				       8,12,10,14, 9,13,11,15 };

/*
 * fb_parserest:
 *
 *	This function will complete the parsing of an escape sequence and
 *	call the appropriate support routine if it matches a character.  This
 *	function could be greatly improved by using a function jump table, and
 *	removing this bulky switch statement.
 *
 * input	: An string
 * output	: Appropriate action based on whether the string matches a
 *	 	  sequence acceptable to the ansi terminal specification
 *
 */
void
fb_parserest(u_char *cp)
{
	int	number[16], npar = 0, i;
	csrpos_t newpos;

	for(i=0;i<=15;i++)
		number[i] = MACH_ATOI_DEFAULT;

	do {
		cp += mach_atoi(cp, &number[npar]);
	} while (*cp == ';' && ++npar <= 15 && cp++);
	
	switch(*cp) {
	case 'm':
		for (i=0;i<=npar;i++)
			switch(number[i]) {
			case MACH_ATOI_DEFAULT:
			case 0:
				fb_attrflags = 0;
				fb_color = KA_NORMAL;
				break;
			case 1:
				fb_attrflags |= KAX_BOLD;
				fb_attrflags &= ~KAX_DIM;
				break;
			case 2:
				fb_attrflags |= KAX_DIM;
				fb_attrflags &= ~KAX_BOLD;
				break;
			case 4:
				fb_attrflags |= KAX_UNDERLINE;
				break;
			case 5:
				fb_attrflags |= KAX_BLINK;
				break;
			case 7:
				fb_attrflags |= KAX_REVERSE;
				break;
			case 8:
				fb_attrflags |= KAX_INVISIBLE;
				break;
			case 21:
			case 22:
				fb_attrflags &= ~(KAX_BOLD | KAX_DIM);
				break;
			case 24:
				fb_attrflags &= ~KAX_UNDERLINE;
				break;
			case 25:
				fb_attrflags &= ~KAX_BLINK;
				break;
			case 27:
				fb_attrflags &= ~KAX_REVERSE;
				break;
			case 38:
				fb_attrflags |= KAX_UNDERLINE;
				fb_color = (fb_color & 0xf0) | (KA_NORMAL & 0x0f);
				break;
			case 39:
				fb_attrflags &= ~KAX_UNDERLINE;
				fb_color = (fb_color & 0xf0) | (KA_NORMAL & 0x0f);
				break;
			default:
			  if (number[i] >= 30 && number[i] <= 37) {
			    /* foreground color */
			    fb_color = (fb_color & 0xf0) | fb_color_table[(number[i] - 30)];
			  } else if (number[i] >= 40 && number[i] <= 47) {
			    /* background color */
			    fb_color = (fb_color & 0x0f) | (fb_color_table[(number[i] - 40)] << 4);
			  }
			  break;
			}
		fb_update_fb_attr();
		fb_esc_spt = fb_esc_seq;
		break;
	case '@':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_insch(1);
		else
			fb_insch(number[0]);
		fb_esc_spt = fb_esc_seq;
		break;
	case 'A':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_up();
		else
			while (number[0]--)
				fb_up();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'B':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_down();
		else
			while (number[0]--)
				fb_down();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'C':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_right();
		else
			while (number[0]--)
				fb_right();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'D':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_left();
		else
			while (number[0]--)
				fb_left();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'E':
		fb_cr();
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_down();
		else
			while (number[0]--)
				fb_down();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'F':
		fb_cr();
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_up();
		else
			while (number[0]--)
				fb_up();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'G':
		if (number[0] == MACH_ATOI_DEFAULT)
			number[0] = 0;
		else
			if (number[0] > 0)
				--number[0];	/* because number[0] is from 1 */
		fb_setpos(BEG_OF_LINE(fb_curpos) + number[0] * ONE_SPACE);
		fb_esc_spt = fb_esc_seq;
		break;
	case 'f':
	case 'H':
		if (number[0] == MACH_ATOI_DEFAULT && number[1] == MACH_ATOI_DEFAULT)
		{
			fb_home();
			fb_esc_spt = fb_esc_seq;
			break;
		}
		if (number[0] == MACH_ATOI_DEFAULT)
			number[0] = 0;
		else if (number[0] > 0)
			--number[0];		/* numbered from 1 */
		newpos = (number[0] * ONE_LINE);   /* setup row */
		if (number[1] == MACH_ATOI_DEFAULT)
			number[1] = 0;
		else if (number[1] > 0)
			number[1]--;
		newpos += (number[1] * ONE_SPACE);	/* setup column */
		if (newpos < 0)
			newpos = 0;		/* upper left */
		if (newpos > ONE_PAGE)
			newpos = (ONE_PAGE - ONE_SPACE); /* lower right */
		fb_setpos(newpos);
		fb_esc_spt = fb_esc_seq;
		break;				/* done or not ready */
	case 'J':
		switch(number[0]) {
		case MACH_ATOI_DEFAULT:
		case 0:
			fb_cltobcur();	/* clears from current
					   pos to bottom.
					   */
			break;
		case 1:
			fb_cltopcur();	/* clears from top to
					   current pos.
					   */
			break;
		case 2:
			fb_cls();
			break;
		default:
			break;
		}
		fb_esc_spt = fb_esc_seq;		/* reset it */
		break;
	case 'K':
		switch(number[0]) {
		case MACH_ATOI_DEFAULT:
		case 0:
			fb_cltoecur();	/* clears from current
					   pos to eoln.
					   */
			break;
		case 1:
			fb_clfrbcur();	/* clears from begin
					   of line to current
					   pos.
					   */
			break;
		case 2:
			fb_eraseln();	/* clear entire line */
			break;
		default:
			break;
		}
		fb_esc_spt = fb_esc_seq;
		break;
	case 'L':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_insln(1);
		else
			fb_insln(number[0]);
		fb_esc_spt = fb_esc_seq;
		break;
	case 'M':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_delln(1);
		else
			fb_delln(number[0]);
		fb_esc_spt = fb_esc_seq;
		break;
	case 'P':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_delch(1);
		else
			fb_delch(number[0]);
		fb_esc_spt = fb_esc_seq;
		break;
	case 'S':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_scrollup();
		else
			while (number[0]--)
				fb_scrollup();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'T':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_scrolldn();
		else
			while (number[0]--)
				fb_scrolldn();
		fb_esc_spt = fb_esc_seq;
		break;
	case 'X':
		if (number[0] == MACH_ATOI_DEFAULT)
			fb_erase(1);
		else
			fb_erase(number[0]);
		fb_esc_spt = fb_esc_seq;
		break;
	case '\0':
		break;			/* not enough yet */
	default:
		fb_putc(*cp);		/* show inv character */
		fb_esc_spt = fb_esc_seq;	/* inv entry, reset */
		break;
	}
	return;
}

void
fb_tab(void)
{
    int i;

    for (i = 8 - (CURRENT_COLUMN(fb_curpos) % 8); i > 0; i--) {
	fb_putc(' ');
    }

}


/*
 * fb_cls:
 *
 *	This function clears the screen with spaces and the current attribute.
 *
 * input	: None
 * output	: Screen is cleared
 *
 */
void
fb_cls(void)
{
	fbclear(0, ONE_PAGE/ONE_SPACE, fb_attr);
	return;
}


/*
 * fb_home:
 *
 *	This function will move the cursor to the home position on the screen,
 *	as well as set the internal cursor position (fb_curpos) to home.
 *
 * input	: None
 * output	: Cursor position is moved
 *
 */
void
fb_home(void)
{
	fb_setpos(0);
	return;
}


/*
 * fb_up:
 *
 *	This function moves the cursor up one line position.
 *
 * input	: None
 * output	: Cursor moves up one line, or screen is scrolled
 *
 */
void
fb_up(void)
{
	if (fb_curpos < ONE_LINE)
		fb_scrolldn();
	else
		fb_setpos(fb_curpos - ONE_LINE);
	return;
}


/*
 * fb_down:
 *
 *	This function moves the cursor down one line position.
 *
 * input	: None
 * output	: Cursor moves down one line or the screen is scrolled
 *
 */
void
fb_down(void)
{
	if (fb_curpos >= (ONE_PAGE - ONE_LINE))
		fb_scrollup();
	else
		fb_setpos(fb_curpos + ONE_LINE);
	return;
}


/*
 * fb_right:
 *
 *	This function moves the cursor one position to the right.
 *
 * input	: None
 * output	: Cursor moves one position to the right
 *
 */
void
fb_right(void)
{
	if (fb_curpos < (ONE_PAGE - ONE_SPACE))
		fb_setpos(fb_curpos + ONE_SPACE);
	else {
		fb_scrollup();
		fb_setpos(BEG_OF_LINE(fb_curpos));
	}
	return;
}


/*
 * fb_left:
 *
 *	This function moves the cursor one position to the left.
 *
 * input	: None
 * output	: Cursor moves one position to the left
 *
 */
void
fb_left(void)
{
	if (0 < fb_curpos)
		fb_setpos(fb_curpos - ONE_SPACE);
	return;
}


/*
 * fb_cr:
 *
 *	This function moves the cursor to the beginning of the current
 *	line.
 *
 * input	: None
 * output	: Cursor moves to the beginning of the current line
 *
 */
void
fb_cr(void)
{
	fb_setpos(BEG_OF_LINE(fb_curpos));
	return;
}


/*
 * fb_cltobcur:
 *
 *	This function clears from the current cursor position to the bottom
 *	of the screen.
 *
 * input	: None
 * output	: Screen is cleared from current cursor position to bottom
 *
 */
void
fb_cltobcur(void)
{
	csrpos_t start;
	int	count;

	start = fb_curpos;
	count = (ONE_PAGE - fb_curpos)/ONE_SPACE;
	fbclear(start, count, fb_attr);
	return;
}


/*
 * fb_cltopcur:
 *
 *	This function clears from the current cursor position to the top
 *	of the screen.
 *
 * input	: None
 * output	: Screen is cleared from current cursor position to top
 *
 */
void
fb_cltopcur(void)
{
	int	count;

	count = (fb_curpos + ONE_SPACE) / ONE_SPACE;
	fbclear(0, count, fb_attr);
	return;
}


/*
 * fb_cltoecur:
 *
 *	This function clears from the current cursor position to eoln.
 *
 * input	: None
 * output	: Line is cleared from current cursor position to eoln
 *
 */
void
fb_cltoecur(void)
{
	csrpos_t i;
	csrpos_t hold;

	hold = BEG_OF_LINE(fb_curpos) + ONE_LINE;
	for (i = fb_curpos; i < hold; i += ONE_SPACE) {
		fbput(i, K_SPACE, fb_attr);
	}
}


/*
 * fb_clfrbcur:
 *
 *	This function clears from the beginning of the line to the current
 *	cursor position.
 *
 * input	: None
 * output	: Line is cleared from beginning to current position
 *
 */
void
fb_clfrbcur(void)
{
	csrpos_t i;

	for (i = BEG_OF_LINE(fb_curpos); i <= fb_curpos; i += ONE_SPACE) {
		fbput(i, K_SPACE, fb_attr);
	}
}


/*
 * fb_delln:
 *
 *	This function deletes 'number' lines on the screen by effectively
 *	scrolling the lines up and replacing the old lines with spaces.
 *
 * input	: number of lines to delete
 * output	: lines appear to be deleted
 *
 */
void
fb_delln(int number)
{
	csrpos_t to;
	csrpos_t from;
	int	delbytes;		/* num of bytes to delete */
	int	count;			/* num of words to move or fill */

	if (number <= 0)
		return;

	delbytes = number * ONE_LINE;
	to = BEG_OF_LINE(fb_curpos);
	if (to + delbytes >= ONE_PAGE)
		delbytes = ONE_PAGE - to;
	if (to + delbytes < ONE_PAGE) {
		from = to + delbytes;
		count = (ONE_PAGE - from) / ONE_SPACE;
		fbmvup(from, to, count);
	}

	to = ONE_PAGE - delbytes;
	count = delbytes / ONE_SPACE;
	fbclear(to, count, fb_attr);
	return;
}


/*
 * fb_insln:
 *
 *	This function inserts a line above the current one by
 *	scrolling the current line and all the lines below it down.
 *
 * input	: number of lines to insert
 * output	: New lines appear to be inserted
 *
 */
void
fb_insln(int number)
{
	csrpos_t to;
	csrpos_t from;
	int	count;
	csrpos_t top;			/* top of block to be moved */
	int	insbytes;		/* num of bytes inserted */

	if (number <= 0)
		return;

	top = BEG_OF_LINE(fb_curpos);
	insbytes = number * ONE_LINE;
	if (top + insbytes > ONE_PAGE)
		insbytes = ONE_PAGE - top;
	to = ONE_PAGE - ONE_SPACE;
	from = to - insbytes;
	if (from > top) {
		count = (from - top + ONE_SPACE) / ONE_SPACE;
		fbmvdown(from, to, count);
	}

	count = insbytes / ONE_SPACE;
	fbclear(top, count, fb_attr);
	return;
}


/*
 * fb_delch:
 *
 *	This function deletes a number of characters from the current
 *	position in the line.
 *
 * input	: number of characters to delete
 * output	: characters appear to be deleted
 *
 */
void
fb_delch(int number)
{
	int	 count;			/* num words moved/filled */
	int	 delbytes;		/* bytes to delete */
	csrpos_t to;
	csrpos_t from;
	csrpos_t nextline;		/* start of next line */

	if (number <= 0)
		return;

	nextline = BEG_OF_LINE(fb_curpos) + ONE_LINE;
	delbytes = number * ONE_SPACE;
	if (fb_curpos + delbytes > nextline)
		delbytes = nextline - fb_curpos;
	if (fb_curpos + delbytes < nextline) {
		from = fb_curpos + delbytes;
		to = fb_curpos;
		count = (nextline - from) / ONE_SPACE;
		fbmvup(from, to, count);
	}

	to = nextline - delbytes;
	count = delbytes / ONE_SPACE;
	fbclear(to, count, fb_attr);
	return;

}


/*
 * fb_erase:
 *
 *	This function overwrites characters with a space starting with the
 *	current cursor position and ending in number spaces away.
 *
 * input	: number of characters to erase
 * output	: characters appear to be blanked or erased
 *
 */
void
fb_erase(int number)
{
	csrpos_t i;
	csrpos_t stop;

	stop = fb_curpos + (ONE_SPACE * number);
	if (stop > BEG_OF_LINE(fb_curpos) + ONE_LINE)
		stop = BEG_OF_LINE(fb_curpos) + ONE_LINE;
	for (i = fb_curpos; i < stop; i += ONE_SPACE) {
		fbput(i, K_SPACE, fb_attr);
	}
	return;
}


/*
 * fb_eraseln:
 *
 *	This function erases the current line with spaces.
 *
 * input	: None
 * output	: Current line is erased
 *
 */
void
fb_eraseln(void)
{
	csrpos_t i;
	csrpos_t stop;

	stop = BEG_OF_LINE(fb_curpos) + ONE_LINE;
	for (i = BEG_OF_LINE(fb_curpos); i < stop; i += ONE_SPACE) {
		fbput(i, K_SPACE, fb_attr);
	}
	return;
}


/*
 * fb_insch:
 *
 *	This function inserts a blank at the current cursor position
 *	and moves all other characters on the line over.
 *
 * input	: number of blanks to insert
 * output	: Blanks are inserted at cursor position
 *
 */
void
fb_insch(int number)
{
	csrpos_t to;
	csrpos_t from;
	int	count;
	csrpos_t nextline;		/* start of next line */
	int	insbytes;		/* num of bytes inserted */

	if (number <= 0)
		return;

	nextline = BEG_OF_LINE(fb_curpos) + ONE_LINE;
	insbytes = number * ONE_SPACE;
	if (fb_curpos + insbytes > nextline)
		insbytes = nextline - fb_curpos;

	to = nextline - ONE_SPACE;
	from = to - insbytes;
	if (from >= fb_curpos) {
		count = (from - fb_curpos + ONE_SPACE) / ONE_SPACE;
		fbmvdown(from, to, count);
	}

	count = insbytes / ONE_SPACE;
	fbclear(fb_curpos, count, fb_attr);
	return;
}


/*
 * fb_isupper, fb_islower:
 *
 *	Didn't want to include ctype.h because it brings in stdio.h, and
 *	only want to see if the darn character is uppercase or lowercase.
 *
 * input	: Character 'c'
 * output	: isuuper gives TRUE if character is uppercase, islower
 *		  returns TRUE if character is lowercase
 *
 */
boolean_t
fb_isupper(u_char c)
{
	if (('A' <= c) && (c <= 'Z'))
		return(TRUE);
	return(FALSE);
}

boolean_t
fb_islower(u_char c)
{
	if (('a' <= c) && (c <= 'z'))
		return(TRUE);
	return(FALSE);
}

#if 0

/*
 * fb_senddata:
 *
 *	This function sends a byte to the keyboard RDWR port, but
 *	first waits until the input/output data buffer is clear before
 *	sending the data.  Note that this byte can be either data or a
 *	keyboard command.
 *
 */
void
fb_senddata(unsigned char ch)
{
	while (inb(K_STATUS) & K_IBUF_FUL)
		;
	outb(K_RDWR, ch);
	last_sent = ch;
	return;
}

/*
 * fb_sendcmd:
 *
 *	This function sends a command byte to the keyboard command
 *	port, but first waits until the input/output data buffer is
 *	clear before sending the data.
 *
 */
void
fb_sendcmd(unsigned char ch)
{
	while (inb(K_STATUS) & K_IBUF_FUL)
		;
	outb(K_CMD, ch);
	return;
}


/*
 * fb_getdata:
 *
 *	This function returns a data byte from the keyboard RDWR port,
 *	after waiting until the port is flagged as having something to
 *	read.
 */
unsigned char
fb_getdata(void)
{
	while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
		;
	return(inb(K_RDWR));
}

unsigned char
fb_cmdreg_read(void)
{
int ch=KC_CMD_READ;

	while (inb(K_STATUS) & K_IBUF_FUL)
		;
	outb(K_CMD, ch);

	while ((inb(K_STATUS) & K_OBUF_FUL) == 0)
		;
	return(inb(K_RDWR));
}

void
fb_cmdreg_write(int val)
{
int ch=KC_CMD_WRITE;

	while (inb(K_STATUS) & K_IBUF_FUL)
		;
	outb(K_CMD, ch);

	while (inb(K_STATUS) & K_IBUF_FUL)
		;
	outb(K_RDWR, val);
}

void
fb_mouse_drain(void)
{
	int i;
	while(inb(K_STATUS) & K_IBUF_FUL)
		;
	while((i = inb(K_STATUS)) & K_OBUF_FUL)
		printf("kbd: S = %x D = %x\n", i, inb(K_RDWR));
}

/*
 * set_fb_state:
 *
 *	Set fb_state and update the keyboard status LEDs.
 */
void
set_fb_state(int newstate)
{
	fb_state = newstate;
	fb_setleds1(state2leds(newstate));
}

/*
 * state2leds:
 *
 *	Return a byte containing LED settings for the keyboard, given
 *	a state vector.
 */
u_char
state2leds(int state)
{
	u_char result = 0;

	if (state & KS_NLKED)
		result |= K_LED_NUMLK;
	if (state & KS_CLKED)
		result |= K_LED_CAPSLK;
	return(result);
}

/*
 * fb_setleds[12]:
 *
 *	Set the keyboard LEDs according to the given byte.
 */
void
fb_setleds1(u_char val)
{
	if (fb_ack != NOT_WAITING) {
#ifdef MACH_KBD
		printf("fb_setleds1: unexpected state (%d)\n", fb_ack);
#endif
		return;
	}

	fb_ack = SET_LEDS;
	fb_nextled = val;
	fb_senddata(K_CMD_LEDS);
}

void
fb_setleds2(void)
{
	fb_senddata(fb_nextled);
}


/*
 * cnsetleds:
 *
 *	like fb_setleds[12], but not interrupt-based.
 *	Currently disabled because cngetc ignores caps lock and num
 *	lock anyway.
 */
void
cnsetleds(u_char val)
{
	fb_senddata(K_CMD_LEDS);
	(void)fb_getdata();		/* XXX - assume is ACK */
	fb_senddata(val);
	(void)fb_getdata();		/* XXX - assume is ACK */
}

void
fbreboot(void)
{
	(*fb_dreset)();

#ifndef BROKEN_KEYBOARD_RESET
	fb_sendcmd(0xFE);		/* XXX - magic # */
	delay(1000000);			/* wait to see if anything happens */
#endif
	/*
	 * If that didn't work, then we'll just have to try and
	 * do it the hard way.
	 */
	cpu_shutdown();
}

static int which_button[] = {0, MOUSE_LEFT, MOUSE_MIDDLE, MOUSE_RIGHT};
static struct mouse_motion moved;

int
fb_kbd_magic(int scancode)
{
int new_button = 0;

	if (fb_kbd_mouse == 2)
		printf("sc = %x\n", scancode);

	switch (scancode) {
/* f1 f2 f3 */
	case 0x3d:
		new_button++;
	case 0x3c:
		new_button++;
	case 0x3b:
		new_button++;
		if (fb_kbd_magic_button && (new_button != fb_kbd_magic_button)) {
				/* down w/o up */
			mouse_button(which_button[fb_kbd_magic_button], 1);
		}
				/* normal */
		if (fb_kbd_magic_button == new_button) {
			mouse_button(which_button[new_button], 1);
			fb_kbd_magic_button = 0;
		} else {
			mouse_button(which_button[new_button], 0);
			fb_kbd_magic_button = new_button;
		}
		break;

/* right left up down */
	case 0x4d:
		moved.mm_deltaX = fb_kbd_magic_scale;
		moved.mm_deltaY = 0;
		mouse_moved(moved);
		break;
	case 0x4b:
		moved.mm_deltaX = -fb_kbd_magic_scale;
		moved.mm_deltaY = 0;
		mouse_moved(moved);
		break;
	case 0x48:
		moved.mm_deltaX = 0;
		moved.mm_deltaY = fb_kbd_magic_scale;
		mouse_moved(moved);
		break;
	case 0x50:
		moved.mm_deltaX = 0;
		moved.mm_deltaY = -fb_kbd_magic_scale;
		mouse_moved(moved);
		break;
/* home pageup end pagedown */
	case 0x47:
		moved.mm_deltaX = -2*fb_kbd_magic_scale;
		moved.mm_deltaY = 2*fb_kbd_magic_scale;
		mouse_moved(moved);
		break;
	case 0x49:
		moved.mm_deltaX = 2*fb_kbd_magic_scale;
		moved.mm_deltaY = 2*fb_kbd_magic_scale;
		mouse_moved(moved);
		break;
	case 0x4f:
		moved.mm_deltaX = -2*fb_kbd_magic_scale;
		moved.mm_deltaY = -2*fb_kbd_magic_scale;
		mouse_moved(moved);
		break;
	case 0x51:
		moved.mm_deltaX = 2*fb_kbd_magic_scale;
		moved.mm_deltaY = -2*fb_kbd_magic_scale;
		mouse_moved(moved);
		break;

	default:
		return 0;
	}
	return 1;
}


/*
 * fb_xga_init:
 *
 *	Initialization specific to character-based graphics adapters.
 */
void
fb_xga_init(void)
{
	csrpos_t	xga_getpos();
	unsigned char	screen;
	unsigned char	start, stop;

	outb(CMOS_ADDR, CMOS_EB);
	screen = inb(CMOS_DATA) & CM_SCRMSK;
	switch(screen) {
	default:
		printf("fb: unknown screen type, defaulting to EGA\n");
		/* FALLTHROUGH */
	case CM_EGA_VGA:
		/*
		 * Here we'll want to query to bios on the card
		 * itself, because then we can figure out what
		 * type we have exactly.  At this point we only
		 * know that the card is NOT CGA or MONO.  For
		 * now, however, we assume backwards compatibility
		 * with 0xb8000 as the starting screen offset
		 * memory location for these cards.
		 *
		 */

		vid_start = (u_char *)phystokv(EGA_START);
		fb_index_reg = EGA_IDX_REG;
		fb_io_reg = EGA_IO_REG;
		fb_lines = 25;
		fb_cols = 80;
		fb_bitmap_start = 0xa0000; /* XXX - magic numbers */
		{		/* XXX - is there a cleaner way to do this? */
		    char *addr = (char *)phystokv(fb_bitmap_start);
		    int i;
		    for (i = 0; i < 200; i++)
			addr[i] = 0x00;
		}
		break;
#if 0
	/* XXX: some buggy BIOSes report these...  */
	case CM_CGA_40:
		vid_start = (u_char *)phystokv(CGA_START);
		fb_index_reg = CGA_IDX_REG;
		fb_io_reg = CGA_IO_REG;
		fb_lines = 25;
		fb_cols = 40;
		break;
	case CM_CGA_80:
		vid_start = (u_char *)phystokv(CGA_START);
		fb_index_reg = CGA_IDX_REG;
		fb_io_reg = CGA_IO_REG;
		fb_lines = 25;
		fb_cols = 80;
		break;
	case CM_MONO_80:
		vid_start = (u_char *)phystokv(MONO_START);
		fb_index_reg = MONO_IDX_REG;
		fb_io_reg = MONO_IO_REG;
		fb_lines = 25;
		fb_cols = 80;
		break;
#endif
	}

	outb(fb_index_reg, C_START);
	start = inb(fb_io_reg);
	/* Make sure cursor is enabled */
	start &= ~0x20;
	outb(fb_io_reg, start);
	outb(fb_index_reg, C_STOP);
	stop = inb(fb_io_reg);

	if (!start && !stop)
	{
		/* Some firmware seem not to be initializing the cursor size
		 * any more...  Try using standard values.  */
		outb(fb_index_reg, C_START);
		outb(fb_io_reg, 14);
		outb(fb_index_reg, C_STOP);
		outb(fb_io_reg, 15);
	}

	fb_setpos(xga_getpos());
}


/*
 * xga_getpos:
 *
 *	This function returns the current hardware cursor position on the
 *	screen, scaled for compatibility with fb_curpos.
 *
 * input	: None
 * output	: returns the value of cursor position on screen
 *
 */
csrpos_t
xga_getpos(void)

{
	unsigned char	low;
	unsigned char	high;
	short pos;

	outb(fb_index_reg, C_HIGH);
	high = inb(fb_io_reg);
	outb(fb_index_reg, C_LOW);
	low = inb(fb_io_reg);
	pos = (0xff&low) + ((unsigned short)high<<8);

	return(ONE_SPACE * (csrpos_t)pos);
}


/*
 * charput:
 *
 *	Put attributed character for EGA/CGA/etc.
 */
static void
charput(pos, ch, chattr)
csrpos_t pos;				/* where to put it */
char	ch;				/* the character */
char	chattr;				/* its attribute */
{
	*(vid_start + pos) = ch;
	*(vid_start + pos + 1) = chattr;
}


/*
 * charsetcursor:
 *
 *	Set hardware cursor position for EGA/CGA/etc.
 */
static void
charsetcursor(newpos)
csrpos_t newpos;
{
	short curpos;		/* position, not scaled for attribute byte */

    	curpos = newpos / ONE_SPACE;
    	outb(fb_index_reg, C_HIGH);
    	outb(fb_io_reg, (u_char)(curpos>>8));
    	outb(fb_index_reg, C_LOW);
    	outb(fb_io_reg, (u_char)(curpos&0xff));

	fb_curpos = newpos;
}


/*
 * charmvup:
 *
 *	Block move up for EGA/CGA/etc.
 */
static void
charmvup(from, to, count)
csrpos_t from, to;
int count;
{
	fb_slmscu(vid_start+from, vid_start+to, count);
}


/*
 * charmvdown:
 *
 *	Block move down for EGA/CGA/etc.
 */
static void
charmvdown(from, to, count)
csrpos_t from, to;
int count;
{
	fb_slmscd(vid_start+from, vid_start+to, count);
}


/*
 * charclear:
 *
 *	Fast clear for CGA/EGA/etc.
 */
static void
charclear(to, count, chattr)
csrpos_t to;
int	count;
char	chattr;
{
	fb_slmwd(vid_start+to, count, ((unsigned short)chattr<<8)+K_SPACE);
}

/*
 * fb_noopreset:
 *
 * 	No-op reset routine for fb_dreset.
 */
static void
fb_noopreset(void)
{
}

#endif /* 0 */


/*
 * fbput: Copy a character from the font to the frame buffer.
 */

void
fbput(
	csrpos_t pos,
	char	 ch, 
	char	 chattr)
{
	short xpix, ypix;		/* u/l corner of char pos */
	short i, j;
	uint32_t val = (chattr == KA_REVERSE ? fb_char_black : fb_char_white);
	uint32_t cval = (chattr == KA_REVERSE ? fb_char_white : fb_char_black);

	if ((u_char)ch >= fb_chars_in_font)
		ch = K_QUES;

	fbch2pix(pos, &xpix, &ypix);
	for (i = 0; i < fb_char_height; ++i) {
		for (j = 0; j < fb_char_width; ++j) {
			if (fb_font[(uint8_t)ch][i] & (1 << j))
				fbpaintpix(xpix + j, ypix + i, val);
			else
				fbpaintpix(xpix + j, ypix + i, cval);
		}
	}
}

/*
 * fbcp1char: copy 1 char from one place in the frame buffer to
 * another.
 */
void
fbcp1char(
	csrpos_t from,
	csrpos_t to)
{
	short from_xpix, from_ypix, from_offset;
	short to_xpix, to_ypix, to_offset, to_offset_end, offset_diff;
	uint32_t *tp, *fp;
	short i, j;
	short num_words;
	uint32_t in_buf1 = 0, in_buf2 = 0, mask = 0;

	fbch2pix(from, &from_xpix, &from_ypix);
	fbch2pix(to, &to_xpix, &to_ypix);

	tp = (uint32_t *)fbpix2ptr(to_xpix, to_ypix, &to_offset);
	to_offset_end = to_offset + fb_char_width*fb_bit_depth;
	num_words = to_offset_end / 32;
	to_offset_end %= 32;
	fp = (uint32_t *)fbpix2ptr(from_xpix, from_ypix, &from_offset);
	/*from_offset_end = from_offset + fb_char_width*fb_bit_depth;
	from_offset_end %= 32;*/

	for (i = 0; i < fb_char_height; ++i) {
		if (to_offset == from_offset) {
			mask = (1 << to_offset) - 1;
			*tp = (*tp & mask) | (*fp & ~mask);

			for (j = 1; j < num_words; ++j)
				tp[j] = fp[j];

			mask = (1 << to_offset_end) - 1;
			*(tp+num_words) = (*(tp+num_words) & ~mask) | (*(fp+num_words) & mask);
			
		}
		else {
			offset_diff = from_offset > to_offset ? from_offset - to_offset : 32 - from_offset + to_offset;
			in_buf1 = (*fp) << offset_diff;
			if (offset_diff > 0) {
				in_buf1 = (*fp << offset_diff) | (*(fp+1) >> (32 - offset_diff));
				fp++;
			}
			else {
				in_buf1 = (*(fp) >> (32 - offset_diff));
			}
			in_buf2 = *fp << offset_diff;
			fp++;
			
			mask = (1 << to_offset) - 1;
			*tp = (*tp & mask) | (in_buf1 & ~mask);
			in_buf1 = in_buf2 | *fp >> (32 - offset_diff);
			in_buf2 = *fp << offset_diff;			
			fp++;

			for (j = 1; j < num_words; ++j) {
				tp[j] = in_buf1;
				in_buf1 = in_buf2 | *fp >> (32 - offset_diff);
				in_buf2 = *fp << offset_diff;			
				fp++;
			}
			
			mask = (1 << to_offset_end) - 1;
			*(tp+num_words) = (*(tp+num_words) & ~mask) | (in_buf1 & mask);
		}
		tp += fb_fb_byte_width;
		fp += fb_fb_byte_width;
	}
}

/*
 * fbvmup: Copy a block of character positions upwards.
 */
void
fbmvup(
	csrpos_t 	from, 
	csrpos_t	to,
	int		count)
{
	short i;

	for (i=0; i < count; ++i) {
		fbcp1char(from, to);
		from++;
		to++;
	}
}

/*
 * fbmvdown: copy a block of characters down.
 */
void
fbmvdown(
	csrpos_t 	from, 
	csrpos_t	to,
	int		count)
{
	short i;
	
	for (i=0; i < count; ++i) {
		fbcp1char(from, to);
		from--;
		to--;
	}
}

/*
 * fbclear: clear one or more character positions.
 */
void
fbclear(
	csrpos_t 	to,				/* 1st char */
	int		count,				/* num chars */
	char		chattr)				/* reverse or normal */
{
	short i;

	for (i = 0; i < count; ++i) {
		fbput(to, K_SPACE, chattr);
		to++;
	}
}

/*
 * fbsetcursor: update the display and set the logical cursor.
 */
void
fbsetcursor(csrpos_t pos)
{
	/* erase old cursor & paint new one */
	fbpaintcsr(fb_curpos, fb_char_black);
	fbpaintcsr(pos, fb_char_white);
	fb_curpos = pos;
}

/*
 * fbpaintcsr: paint cursor pixels.
 */
void
fbpaintcsr(
	csrpos_t 	pos,
	uint32_t	val)
{
	short xpix, ypix, xstart, ystart;

	fbch2pix(pos, &xstart, &ystart);
	ystart += fb_char_height;		/* position at bottom of line */
	for (ypix = ystart; ypix < ystart + fb_cursor_height; ++ypix) {
		for (xpix = xstart; xpix < xstart + fb_char_width; ++xpix) {
			fbpaintpix(xpix,ypix,val);
		}
	}
}

/*
 * fbch2bit: convert character position to x and y bit addresses.
 * (0, 0) is the upper left corner.
 */
void
fbch2pix(
	csrpos_t 	pos,
	short		*xp, 
	short		*yp)			/* x, y pixel positions, u/l corner */
{
	short xch, ych;

	xch = pos % fb_cols;
	ych = pos / (fb_cols);
	*xp = fb_xstart + xch * fb_char_width;
	*yp = fb_ystart + ych * (fb_char_height + fb_cursor_height);
}

/*
 * fbbit2ptr: return a pointer into the frame buffer corresponding to
 * the bit address (x, y).
 * Returns the offset bit if needed.
 */
u_char *
fbpix2ptr(
	short	xp, 
	short	yp,
	short	*bit_offset)
{
	u_char *addr;
	uint32_t long_addr;

	addr = frame_buf_start + yp * fb_fb_byte_width + xp*fb_bit_depth/8;
	long_addr = (uint32_t)addr;
	*bit_offset = xp*fb_bit_depth % 8 + 8*(long_addr & 0x3);
	return((u_char*)(long_addr & 0xFFFFFFFC));
}

void
fbpaintpix(
	short xp,
	short yp,
	uint32_t val)
{
	short start_offset;
	uint32_t start_addr, end_addr;
	uint32_t start_word, end_word, start_mask, end_mask, prep_mask;
	
	start_addr = (uint32_t)frame_buf_start + yp * fb_fb_byte_width + xp*fb_bit_depth/8;
	start_addr &= 0xFFFFFFFC;
	end_addr = (uint32_t)frame_buf_start + yp * fb_fb_byte_width + ((xp+1)*fb_bit_depth-1)/8;
	end_addr &= 0xFFFFFFFC;
	start_offset = (yp * fb_fb_byte_width * 8 + xp * fb_bit_depth) % 32;
	prep_mask = (1 << fb_bit_depth) - 1;

	start_mask = prep_mask << start_offset;
	start_word = val << start_offset;
	end_mask = prep_mask >> (32 - start_offset);
	end_word = val >> (32 - start_offset);
	*((uint32_t *)start_addr) &= (~start_mask);
	*((uint32_t *)start_addr) |= start_word;
	*((uint32_t *)end_addr) &= (~end_mask);
	*((uint32_t *)end_addr) |= end_word;
}
	

/*
 * console stuff
 */

int
fbcnprobe(struct consdev *cp)
{
	int maj, unit, pri;

	maj = 0;
	unit = 0;
	pri = CN_FRAMEBUFFER;
	
	if(multiboot2_get_tag(boot_info,MULTIBOOT2_TAG_FRAMEBUFFER)) {
		cp->cn_dev = makedev(maj, unit);
		cp->cn_pri = pri;
	}
	return 0;
}

int
fbcninit(struct consdev *cp)
{
	fbinit();
	return 0;
}

int
fbcngetc(dev_t dev, int wait)
{
	if (wait) {
		int c;
		while ((c = fbcnmaygetc()) < 0)
			continue;
		return c;
	}
	else
		return fbcnmaygetc();
}

int
fbcnputc(dev_t dev, int c)
{
	if (!fb_initialized)
		return -1;

	/* Note that tab is handled in fb_putc */
	if (c == '\n')
		fb_putc('\r');
	fb_putc_esc(c);

	return 0;
}

/*
 * Just never return a character. The keyboard will not work.
 */
int
fbcnmaygetc(void)
{
	return -1;
}

