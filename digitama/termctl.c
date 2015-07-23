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
uintptr_t NORMAL = A_NORMAL;
uintptr_t STANDOUT = A_STANDOUT;
uintptr_t UNDERLINE = A_UNDERLINE;
uintptr_t REVERSE = A_REVERSE;
uintptr_t BLINK = A_BLINK;
uintptr_t DIM = A_DIM;
uintptr_t BOLD = A_BOLD;
uintptr_t ALTCHARSET = A_ALTCHARSET;

/**
 * The next two are subject to change
 * so don't depend on them.
 **/
uintptr_t INVIS = A_INVIS;
uintptr_t PROTECT = A_PROTECT;

/**
 * The next three are combined as `chtype`
 */
uintptr_t ATTRIBUTES = A_ATTRIBUTES;
uintptr_t CHARTEXT = A_CHARTEXT;
uintptr_t COLOR = A_COLOR;

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

