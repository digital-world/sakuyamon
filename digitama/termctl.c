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

/* Various video attributes */
uintptr_t STANDOUT = A_STANDOUT;
uintptr_t UNDERLINE = A_UNDERLINE;
uintptr_t REVERSE = A_REVERSE;
uintptr_t BLINK = A_BLINK;
uintptr_t DIM = A_DIM;
uintptr_t BOLD = A_BOLD;
uintptr_t ALTCHARSET = A_ALTCHARSET;

uintptr_t INVIS = A_INVIS;     /* The next two are subject to change */
uintptr_t PROTECT = A_PROTECT; /* so don't depend on them. */

uintptr_t NORMAL = A_NORMAL;
uintptr_t ATTRIBUTES = A_ATTRIBUTES;
uintptr_t CHARTEXT = A_CHARTEXT;

uintptr_t color_pair(intptr_t n) {
    return COLOR_PAIR(n);
}

intptr_t pair_number(uintmax_t n) {
    return PAIR_NUMBER(n);
}

/* Colors */
short BLACK = COLOR_BLACK;
short RED = COLOR_RED;
short GREEN = COLOR_GREEN;
short YELLOW = COLOR_YELLOW;
short BLUE = COLOR_BLUE;
short MAGENTA = COLOR_MAGENTA;
short CYAN = COLOR_CYAN;
short WHITE = COLOR_WHITE;

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

