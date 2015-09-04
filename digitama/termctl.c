/* System Headers */
#include <stdio.h>
#include <stdlib.h>

#define _XOPEN_SOURCE_EXTENDED
#if defined(__sun) && defined(__SVR4)
#define _POSIX_C_SOURCE 199506L
#include <ncurses/ncurses.h> /* ld: (ncurses) */
#else
#include <ncurses.h>
#endif

/* return code */
const int OKAY = OK;
const int ERROR = ERR;

/* `chtype` masks */
const attr_t ATTRIBUTES = A_ATTRIBUTES;
const attr_t CHARTEXT = A_CHARTEXT;
const attr_t COLORPAIR = A_COLOR;

attr_t color_pair(short n) {
    return COLOR_PAIR(n);
}

short pair_number(attr_t c) {
    return PAIR_NUMBER(c);
}

/* `chtype` attributes, ordered from low-bit to high-bit */
const attr_t NORMAL = A_NORMAL;
const attr_t NONE = A_NORMAL;           /* vim compatible */
const attr_t STANDOUT = A_STANDOUT;
const attr_t UNDERLINE = A_UNDERLINE;
const attr_t UNDERCURL = A_UNDERLINE;   /* vim compatible */
const attr_t REVERSE = A_REVERSE;
const attr_t INVERSE = A_REVERSE;       /* vim compatible */
const attr_t BLINK = A_BLINK;
const attr_t DIM = A_DIM;
const attr_t BOLD = A_BOLD;
const attr_t ALTCHARSET = A_ALTCHARSET;
const attr_t INVIS = A_INVIS;           /* invisible, subject to change */
const attr_t PROTECT = A_PROTECT;       /* subject to change */
const attr_t HORIZONTAL = A_HORIZONTAL;
const attr_t LEFT = A_LEFT;
const attr_t LOW = A_LOW;
const attr_t RIGHT = A_RIGHT;
const attr_t TOP = A_TOP;
const attr_t VERTICAL = A_VERTICAL;

/**
 * Human readable names for the most commonly used characters.
 * "Upper", "right", etc. are chosen to be consistent with the vt100 manual.
 */

attr_t initailizer_element_should_be_constant(const char *key) {
    size_t key_maxlen;

    key_maxlen = 9;

    if (strncmp(key, "ULCORNER", key_maxlen) == 0) return ACS_ULCORNER; /* upper-left corner */
    if (strncmp(key, "LLCORNER", key_maxlen) == 0) return ACS_LLCORNER; /* low-left corner */
    if (strncmp(key, "URCORNER", key_maxlen) == 0) return ACS_URCORNER; /* upper-right corner */
    if (strncmp(key, "LRCORNER", key_maxlen) == 0) return ACS_LRCORNER; /* low-left corner */
    if (strncmp(key, "RTEE", key_maxlen) == 0) return ACS_RTEE;         /* right T intersection */
    if (strncmp(key, "LTEE", key_maxlen) == 0) return ACS_LTEE;         /* left T intersection */
    if (strncmp(key, "BTEE", key_maxlen) == 0) return ACS_BTEE;         /* bottom T intersection */
    if (strncmp(key, "TTEE", key_maxlen) == 0) return ACS_TTEE;         /* top T intersection */
    if (strncmp(key, "HLINE", key_maxlen) == 0) return ACS_HLINE;       /* horizontal line */
    if (strncmp(key, "VLINE", key_maxlen) == 0) return ACS_VLINE;       /* vertical line */
    if (strncmp(key, "PLUS", key_maxlen) == 0) return ACS_PLUS;         /* plus */
    if (strncmp(key, "S1", key_maxlen) == 0) return ACS_S1;             /* scan line 1 */
    if (strncmp(key, "S3", key_maxlen) == 0) return ACS_S3;             /* scan line 3 */
    if (strncmp(key, "S7", key_maxlen) == 0) return ACS_S3;             /* scan line 7 */   
    if (strncmp(key, "S9", key_maxlen) == 0) return ACS_S9;             /* scan line 9 */
    if (strncmp(key, "DIAMOND", key_maxlen) == 0) return ACS_DIAMOND;   /* diamond */
    if (strncmp(key, "CKBOARD", key_maxlen) == 0) return ACS_CKBOARD;   /* checker board (stipple) */
    if (strncmp(key, "DEGREE", key_maxlen) == 0) return ACS_DEGREE;     /* degree symbol */
    if (strncmp(key, "PLMINUS", key_maxlen) == 0) return ACS_PLMINUS;   /* plus/minus */
    if (strncmp(key, "BULLET", key_maxlen) == 0) return ACS_BULLET;     /* bullet */
    if (strncmp(key, "LARROW", key_maxlen) == 0) return ACS_LARROW;     /* arrow pointing left */
    if (strncmp(key, "RARROW", key_maxlen) == 0) return ACS_RARROW;     /* arrow pointing right */
    if (strncmp(key, "DARROW", key_maxlen) == 0) return ACS_DARROW;     /* arrow pointing down */
    if (strncmp(key, "UARROW", key_maxlen) == 0) return ACS_UARROW;     /* arrow pointing up */
    if (strncmp(key, "BOARD", key_maxlen) == 0) return ACS_BOARD;       /* board of squares */
    if (strncmp(key, "LANTERN", key_maxlen) == 0) return ACS_LANTERN;   /* lantern symbol */
    if (strncmp(key, "BLOCK", key_maxlen) == 0) return ACS_BLOCK;       /* solid square block */
    if (strncmp(key, "LEQUAL", key_maxlen) == 0) return ACS_LEQUAL;     /* less/equal */
    if (strncmp(key, "GEQUAL", key_maxlen) == 0) return ACS_GEQUAL;     /* greater/equal */
    if (strncmp(key, "PI", key_maxlen) == 0) return ACS_PI;             /* Pi */
    if (strncmp(key, "NEQUAL", key_maxlen) == 0) return ACS_NEQUAL;     /* not equal */
    if (strncmp(key, "STERLING", key_maxlen) == 0) return ACS_STERLING; /* UK pound sign */

    return 0;
}

/* 
 * Begin ViM Modeline
 * vim:ft=c:ts=4:
 * End ViM
 */

