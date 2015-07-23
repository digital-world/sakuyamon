#lang at-exp racket

(provide (all-defined-out) c-extern)

(require racket/draw)

@require{posix.rkt}

(define-ffi-definer define-ncurses (ffi-lib "libncurses" #:global? #true))
(define-ffi-definer define-termctl
  (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths))
                       "native" (system-library-subpath #false) "termctl")
           #:global? #true))

(define _window* (_cpointer/null 'WINDOW*))
(define _winpad* (_cpointer/null 'WINDOW*))
(define _ok/err (make-ctype _int #false (lambda [c] (not (eq? c -1)))))

(define _chtype (make-ctype _int char->integer integer->char))

(define-values [_attr _color/term]
  (let*-values ([[symbol-downcase] (compose1 string->symbol string-downcase symbol->string)]
                [[_named-color] ((lambda [c] (_enum c _short))
                                 (foldl (lambda [c C] (list* (symbol-downcase c) '= (c-extern c #:ctype _short) C))
                                        (list 'none '= -1) (list 'BLACK 'RED 'GREEN 'YELLOW 'BLUE 'MAGENTA 'CYAN 'WHITE)))]
                [[named-color->c c->named-color] (values (ctype-scheme->c _named-color) (ctype-c->scheme _named-color))])
    (values ((lambda [a] (_bitmask a _uint))
             (foldl (lambda [a A] (list* (symbol-downcase a) '= (c-extern a #:ctype _uint) A)) null
                    (list 'NORMAL 'STANDOUT 'UNDERLINE 'REVERSE 'BLINK 'DIM 'BOLD
                          'INVIS 'PROTECT 'ALTCHARSET 'ATTRIBUTES 'CHARTEXT)))
            (make-ctype _short
                        (lambda [r] (if (integer? r) r (named-color->c r)))
                        (lambda [c] (if (< c 8) (c->named-color c) c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         WARNING: ncurses uses YX-Coordinate System                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Initialization
(define-ncurses initscr (_fun -> _window*))
(define-ncurses raw (_fun -> _ok/err))
(define-ncurses cbreak (_fun -> _ok/err))
(define-ncurses noecho (_fun -> _ok/err))
(define-ncurses start_color (_fun -> _ok/err))
(define-ncurses keypad (_fun _window* _bool -> _ok/err))
(define-ncurses idlok (_fun _window* _bool -> _ok/err))
(define-ncurses scrollok (_fun _window* _bool -> _ok/err))
(define-ncurses endwin (_fun -> _ok/err))

;;; Window, SubWindow/Pad and Screen functions
(define-ncurses getcury (_fun _window* -> _int))
(define-ncurses getcurx (_fun _window* -> _int))
(define-ncurses getmaxy (_fun _window* -> _int))
(define-ncurses getmaxx (_fun _window* -> _int))

(define-ncurses newwin (_fun [nlines : _int] [ncols : _int] [beginy : _int] [beginx : _int] -> _window*))

(define-ncurses box (_fun _window* _chtype _chtype -> _ok/err))
(define-ncurses wborder (_fun _window* _chtype _chtype _chtype _chtype _chtype _chtype _chtype _chtype -> _ok/err))
(define-ncurses wvline (_fun _window* _chtype _int -> _ok/err))
(define-ncurses whline (_fun _window* _chtype _int -> _ok/err))

(define-ncurses putwin (_fun _path -> _ok/err))
(define-ncurses getwin (_fun _path -> _ok/err))
(define-ncurses overlay (_fun [src : _window*] [dest : _window*] -> _ok/err))
(define-ncurses overwrite (_fun [src : _window*] [dest : _window*] -> _ok/err))
(define-ncurses copywin (_fun [src : _window*] [dest : _window*]
                              [sminrow : _int] [smincol : _int]
                              [dminrow : _int] [dmincol : _int]
                              [dmaxrow : _int] [dmaxcol : _int]
                              [overlay : _bool]
                              -> _ok/err))

(define-ncurses scr_dump (_fun _file -> _ok/err))
(define-ncurses scr_restore (_fun _file -> _ok/err))
(define-ncurses scr_init (_fun _file -> _ok/err))
(define-ncurses scr_set (_fun _file -> _ok/err))

(define-ncurses newpad (_fun [nlines : _int] [ncols : _int] -> _winpad*))
(define-ncurses subpad (_fun [orig : _window*]
                             [nlines : _int] [ncols : _int]
                             [beginy : _int] [beginx : _int]
                             -> _winpad*))

;;; Output functions
(define-ncurses wmove (_fun _window* _int _int -> _ok/err))
(define-ncurses waddstr (_fun _window* _string -> _ok/err))
(define-ncurses mvwaddwstr (_fun _window* _int _int _string/ucs-4 -> _ok/err))
(define-ncurses wrefresh (_fun _window* -> _ok/err))
(define-ncurses wnoutrefresh (_fun _window* -> _ok/err))
(define-ncurses wredrawln (_fun _window* [begin : _int] [n : _int] -> _ok/err))
(define-ncurses doupdate (_fun -> _ok/err))

(define-ncurses prefresh (_fun [pad : _winpad*] ;;; not _window*
                               [pminrow : _int] [pmincol : _int]
                               [sminrow : _int] [smincol : _int]
                               [smaxrow : _int] [smaxcol : _int]
                               -> _ok/err))

(define-ncurses pnoutrefresh (_fun [pad : _winpad*] ;;; not _window*
                                   [pminrow : _int] [pmincol : _int]
                                   [sminrow : _int] [smincol : _int]
                                   [smaxrow : _int] [smaxcol : _int]
                                   -> _ok/err))

;;; Input functions
(define-ncurses wtimeout (_fun _window* _int -> _ok/err)) ;;; blocking mode for getch()
(define-ncurses getch (_fun -> [ascii : _int]
                            -> (with-handlers ([exn? (const #false)])
                                 (integer->char ascii))))

;;; Color and Attribute functions
(define-ncurses has_colors (_fun -> _bool))
(define-ncurses use_default_colors (_fun -> _ok/err))
(define-ncurses assume_default_colors (_fun _color/term _color/term -> _ok/err))

(define color-number (ctype-scheme->c _color/term))

(define-ncurses mvwchgat
  (_fun [win : _window*]
        [line : _int]
        [col : _int]
        [howmany : _int]
        [attrs : _attr]
        [pair_index : _short]
        [opts : _pointer = #false]
        -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In order to reduce the complexity, I prefer (wattr_on) rather than (wattron)  ;;;
;;; (wattron) and friends require OR COLOR_PAIR and ATTRIBUTES                    ;;;
;;; while (wattr_on) only deals with attributes and leaves colors to (wcolor_set) ;;;
;;; both of them works well with (wstandend).                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses wattr_on (_fun _window* _attr [opts : _pointer = #false] -> _ok/err -> #true))
(define-ncurses wattr_off (_fun _window* _attr [opts : _pointer = #false] -> _ok/err -> #true))
(define-ncurses wcolor_set (_fun _window* _color/term [opts : _pointer = #false] -> _ok/err -> #true))
(define-ncurses wstandout (_fun _window* -> _ok/err -> #true))
(define-ncurses wstandend (_fun _window* -> _ok/err -> #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one is a curse, even we have 256 * 256 pair slots, however we cannot use ;;;
;;; them more than 256 - 1 (the first one is reserved as default), because pair   ;;;
;;; No. is combined with attribute and color, the actual storage is only 8bit.    ;;;
;;; It's your duty to manage the palette.                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses init_pair (_fun _short _color/term _color/term -> _ok/err))

(define-ncurses pair_content (_fun [pair : _short] [fg : (_ptr o _color/term)] [bg : (_ptr o _color/term)]
                                   -> [ status : _ok/err] -> (and status (cons fg bg))))

(define _rgb/component
  (make-ctype _short ;;; r/255 = c/1000 <==> 1000r = 255c
              (lambda [r] (exact-round (* r 1000/255))) 
              (lambda [c] (exact-round (* c 255/1000)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one is not recommended because ncurses cannot restore the correct color  ;;;
;;; value after (endwin). All changed colors keep taking effects until you        ;;;
;;; restart your terminal.                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses init_color (_fun _color/term _rgb/component _rgb/component _rgb/component -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one isn't reliable, ncurses just gets palette information from terminfo. ;;;
;;; Now the problem is: 256 colors do work perfectly, but I don't know its RGBs.  ;;;
;;; Again, if colors are changed by application, it might pollute the terminal.   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses color_content
  (_fun [color : _color/term]
        [r/1000 : (_ptr o _rgb/component)] [g/1000 : (_ptr o _rgb/component)] [b/1000 : (_ptr o _rgb/component)]
        -> [status : _ok/err] -> (and status (list r/1000 g/1000 b/1000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; So let this one always return false.                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses can_change_color (_fun -> _bool -> #false))

;;; Miscellaneous
(define-ncurses def_prog_mode (_fun -> _int -> #true))
(define-ncurses reset_prog_mode (_fun -> _int -> #true))
(define-ncurses def_shell_mode (_fun -> _int -> #true))
(define-ncurses reset_shell_mode (_fun -> _int -> #true))



(module+ test
  (provide (all-defined-out))

  (define stdscr (initscr))
  (void ((curry plumber-add-flush! (current-plumber))
         (lambda [this]
           (plumber-flush-handle-remove! this)
           (wrefresh stdscr)
           (getch)
           (endwin)))

        (with-handlers ([exn:fail? (lambda [ef] (and (endwin) (displayln (exn-message ef))))])
          (when (and stdscr (raw) (noecho) (wtimeout stdscr -1) (keypad stdscr #true)
                     (idlok stdscr #true) (scrollok stdscr #true))
            (when (has_colors)
              (start_color)
              (use_default_colors)
              (mvwaddwstr stdscr (getcury stdscr) (getcurx stdscr) (format "256 colors[default: ~a]~n~n" (pair_content 0)))
              (for ([c (in-range (sub1 (c-extern 'COLORS)))])
                (define p (add1 c))
                (init_pair p c 'none)
                (wcolor_set stdscr p)
                (mvwaddwstr stdscr (getcury stdscr) (max 8 (getcurx stdscr)) (~a c #:min-width 4))
                (when (zero? (remainder p 16)) (mvwaddwstr stdscr (getcury stdscr) (getcurx stdscr) "\n"))
                (wstandend stdscr))
              (mvwaddwstr stdscr (getcury stdscr) (getcurx stdscr) (format "~n~nPress any key to exit!"))
              (mvwchgat stdscr (getcury stdscr) 0 -1 (list 'reverse) 0))))))

