/* Header file for fbcons.c */
/* By Colin Parker */

#ifndef	_FBCONS_H_
#define _FBCONS_H

#include <device/conf.h>
#include <device/cons.h>
#include <device/tty.h>
#include <sys/types.h>
#include <sys/ioctl.h>

/* copied from kdsoft.h */

typedef	short	csrpos_t;	/* cursor position, in characters */

/* close copy from kd.h */

#define BOTTOM_LINE (fb_cols*(fb_lines-1))
#define ONE_PAGE (fb_cols*fb_lines)
#define ONE_LINE fb_cols
#define ONE_SPACE 1

#define BEG_OF_LINE(pos) ((pos) - (pos) % ONE_LINE)
#define CURRENT_COLUMN(pos) ((pos) % ONE_LINE)

int  fbopen(dev_t dev, int flag, io_req_t ior);
void fbclose(dev_t dev, int flag);
int  fbread(dev_t dev, io_req_t uio);
int  fbwrite(dev_t dev, io_req_t uio);
int  fbmmap(dev_t dev, vm_offset_t off, vm_prot_t prot);
int  fbportdeath(dev_t dev, mach_port_t port);
io_return_t fbgetstat(dev_t dev, int flavor, int *data, natural_t *count);
io_return_t fbsetstat(dev_t dev, int flavor, int *data, natural_t count);
int  fbsetbell(int val, int flags);
void fb_belloff(void *unused);
void fb_bellon(void);
void fbstart(struct tty *tp);
void fbstop(struct tty *tp, int flags);
void fbinit(void); 
void fb_putc_esc(u_char c);
void fb_putc(u_char ch);
void fb_setpos(csrpos_t newpos);
void fb_scrollup(void);
void fb_scrolldn(void);
void fb_parseesc(void);
void fb_update_fb_attr(void);
void fb_parserest(u_char *cp);
void fb_tab(void);
void fb_cls(void);
void fb_home(void);
void fb_up(void);
void fb_down(void);
void fb_right(void);
void fb_left(void);
void fb_cr(void);
void fb_cltobcur(void);
void fb_cltopcur(void);
void fb_cltoecur(void);
void fb_clfrbcur(void);
void fb_delln(int number);
void fb_insln(int number);
void fb_delch(int number);
void fb_erase(int number);
void fb_eraseln(void);
void fb_insch(int number);
boolean_t fb_islower(u_char c);
boolean_t fb_isupper(u_char c);

void fbput(csrpos_t pos, char h, char chattr);
void fbcp1char(csrpos_t from, csrpos_t to);
void fbmvup(csrpos_t from, csrpos_t to, int count);
void fbmvdown(csrpos_t from, csrpos_t to, int count);
void fbclear(csrpos_t to, int count, char chattr);
void fbsetcursor(csrpos_t pos);
void fbpaintcsr(csrpos_t pos, uint32_t val);
void fbch2pix(csrpos_t pos, short *xp, short *yp);
u_char *fbpix2ptr(short	xp, short yp,short *bit_offset);
void fbpaintpix(short xp, short yp, uint32_t val);

int fbcnprobe(struct consdev *cp);
int fbcninit(struct consdev *cp);
int fbcngetc(dev_t dev, int wait);
int fbcnputc(dev_t dev, int c);
int fbcnmaygetc(void);


/* Below copied from kd.h */

#define FBSETBELL _IOW('k', 4, int)/* turn bell on or off */
#define FB_BELLON 1
#define FB_BELLOFF 0

/*
 * Attributes for character sent to display.
 */
#define KA_NORMAL	0x07
#define KA_REVERSE	0x70

#define KAX_REVERSE	0x01
#define KAX_UNDERLINE	0x02
#define KAX_BLINK	0x04
#define KAX_BOLD	0x08
#define KAX_DIM		0x10
#define KAX_INVISIBLE	0x20

#define KAX_COL_UNDERLINE 0x0f	/* bright white */
#define KAX_COL_DIM 0x08	/* gray */

/* ascii char set */
#define K_NUL		0x00		/* Null character	*/
#define K_SOH		0x01
#define K_STX		0x02
#define K_ETX		0x03
#define K_EOT		0x04
#define K_ENQ		0x05
#define K_ACK		0x06
#define K_BEL		0x07		/* bell character	*/
#define K_BS		0x08		/* back space		*/
#define K_HT		0x09
#define K_LF		0x0a		/* line feed		*/
#define K_VT		0x0b
#define K_FF		0x0c
#define K_CR		0x0d		/* carriage return	*/
#define K_SO		0x0e
#define K_SI		0x0f
#define K_DLE		0x10
#define K_DC1		0x11
#define K_DC2		0x12
#define K_DC3		0x13
#define K_DC4		0x14
#define K_NAK		0x15
#define K_SYN		0x16
#define K_ETB		0x17
#define K_CAN		0x18
#define K_EM		0x19
#define K_SUB		0x1a
#define K_ESC		0x1b		/* escape character	*/
#define K_FS		0x1c
#define K_GS		0x1d
#define K_RS		0x1e
#define K_US		0x1f
#define K_SPACE		0x20		/* space character	*/
#define K_BANG		0x21		/* !			*/
#define K_DQUOTE	0x22		/* "			*/
#define K_POUND		0x23		/* #			*/ 
#define K_DOLLAR	0x24		/* $			*/ 
#define K_PERC		0x25		/* %			*/ 
#define K_AMPER		0x26		/* &			*/ 
#define K_SQUOTE	0x27		/* '			*/ 
#define K_LPAREN	0x28		/* (			*/ 
#define K_RPAREN	0x29		/* )			*/ 
#define K_ASTER		0x2a		/* *			*/ 
#define K_PLUS		0x2b		/* +			*/ 
#define K_COMMA		0x2c		/* ,			*/ 
#define K_MINUS		0x2d		/* -			*/ 
#define K_PERIOD	0x2e		/* .			*/ 
#define K_SLASH		0x2f		/* /			*/ 
#define K_ZERO		0x30		/* 0			*/ 
#define K_ONE		0x31		/* 1			*/
#define K_TWO		0x32		/* 2			*/
#define K_THREE		0x33		/* 3			*/
#define K_FOUR		0x34		/* 4			*/
#define K_FIVE		0x35		/* 5			*/
#define K_SIX		0x36		/* 6			*/
#define K_SEVEN		0x37		/* 7			*/
#define K_EIGHT		0x38		/* 8			*/
#define K_NINE		0x39		/* 9			*/
#define K_COLON		0x3a		/* :			*/
#define K_SEMI		0x3b		/* ;			*/
#define K_LTHN		0x3c		/* <			*/
#define K_EQL		0x3d		/* =			*/
#define K_GTHN		0x3e		/* >			*/
#define K_QUES		0x3f		/* ?			*/
#define K_ATSN		0x40		/* @			*/
#define K_A		0x41		/* A			*/
#define K_B		0x42		/* B			*/
#define K_C		0x43		/* C			*/
#define K_D		0x44		/* D			*/
#define K_E		0x45		/* E			*/
#define K_F		0x46		/* F			*/
#define K_G		0x47		/* G			*/
#define K_H		0x48		/* H			*/
#define K_I		0x49		/* I			*/
#define K_J		0x4a		/* J			*/
#define K_K		0x4b		/* K			*/
#define K_L		0x4c		/* L			*/
#define K_M		0x4d		/* M			*/
#define K_N		0x4e		/* N			*/
#define K_O		0x4f		/* O			*/
#define K_P		0x50		/* P			*/
#define K_Q		0x51		/* Q			*/
#define K_R		0x52		/* R			*/
#define K_S		0x53		/* S			*/
#define K_T		0x54		/* T			*/
#define K_U		0x55		/* U			*/
#define K_V		0x56		/* V			*/
#define K_W		0x57		/* W			*/
#define K_X		0x58		/* X			*/
#define K_Y		0x59		/* Y			*/
#define K_Z		0x5a		/* Z			*/
#define K_LBRKT		0x5b		/* [			*/
#define K_BSLSH		0x5c		/* \			*/
#define K_RBRKT		0x5d		/* ]			*/
#define K_CARET		0x5e		/* ^			*/
#define K_UNDSC		0x5f		/* _			*/
#define K_GRAV		0x60		/* `			*/
#define K_a		0x61		/* a			*/
#define K_b		0x62		/* b			*/
#define K_c		0x63		/* c			*/
#define K_d		0x64		/* d			*/
#define K_e		0x65		/* e			*/
#define K_f		0x66		/* f			*/
#define K_g		0x67		/* g			*/
#define K_h		0x68		/* h			*/
#define K_i		0x69		/* i			*/
#define K_j		0x6a		/* j			*/
#define K_k		0x6b		/* k			*/
#define K_l		0x6c		/* l			*/
#define K_m		0x6d		/* m			*/
#define K_n		0x6e		/* n			*/
#define K_o		0x6f		/* o			*/
#define K_p		0x70		/* p			*/
#define K_q		0x71		/* q			*/
#define K_r		0x72		/* r			*/
#define K_s		0x73		/* s			*/
#define K_t		0x74		/* t			*/
#define K_u		0x75		/* u			*/
#define K_v		0x76		/* v			*/
#define K_w		0x77		/* w			*/
#define K_x		0x78		/* x			*/
#define K_y		0x79		/* y			*/
#define K_z		0x7a		/* z			*/
#define K_LBRACE	0x7b		/* {			*/
#define K_PIPE		0x7c		/* |			*/
#define K_RBRACE	0x7d		/* }			*/
#define K_TILDE		0x7e		/* ~			*/
#define K_DEL		0x7f		/* delete		*/

#endif /* _FBCONS_H_ */