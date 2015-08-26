/* System Headers */
#include <stdio.h>
#include <stdlib.h>

#define _XOPEN_SOURCE_EXTENDED
#if defined(__sun) && defined(__SVR4)
#define _POSIX_C_SOURCE 199506L
#include <ncurses/ncurses.h>
#else
#include <ncurses.h>
#endif

/* `chtype` masks */
uintptr_t ATTRIBUTES = A_ATTRIBUTES;
uintptr_t CHARTEXT = A_CHARTEXT;
uintptr_t COLORPAIR = A_COLOR;

/* `chtype` attributes, ordered from low-bit to high-bit */
uintptr_t NORMAL = A_NORMAL;
uintptr_t NONE = A_NORMAL; /* vim compatible */
uintptr_t STANDOUT = A_STANDOUT;
uintptr_t UNDERLINE = A_UNDERLINE;
uintptr_t UNDERCURL = A_UNDERLINE; /* vim compatible */
uintptr_t REVERSE = A_REVERSE;
uintptr_t INVERSE = A_REVERSE; /* vim compatible */
uintptr_t BLINK = A_BLINK;
uintptr_t DIM = A_DIM;
uintptr_t BOLD = A_BOLD;
uintptr_t ALTCHARSET = A_ALTCHARSET;
uintptr_t INVIS = A_INVIS; /* invisible, subject to change */
uintptr_t PROTECT = A_PROTECT; /* subject to change */
uintptr_t HORIZONTAL = A_HORIZONTAL;
uintptr_t LEFT = A_LEFT;
uintptr_t LOW = A_LOW;
uintptr_t RIGHT = A_RIGHT;
uintptr_t TOP = A_TOP;
uintptr_t VERTICAL = A_VERTICAL;

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */
