#lang at-exp racket

(provide (all-defined-out))

(require file/sha1)

@require{posix.rkt}

(define ncurses (ffi-lib "libncurses" #:global? #true))
(define-ffi-definer define-ncurses ncurses)

(define _window* (_cpointer/null 'WINDOW*))
(define _winpad* (_cpointer/null 'WINDOW*))
(define _ok/err (make-ctype _int #false (lambda [c] (not (eq? c -1)))))

(define ncurs-extern-var
  (lambda [variable-name]
    (define ctype
      (case variable-name
        [(stdscr curscr) _window*]
        [else #|LINES COLS TABSIZE COLORS COLOR_PAIRS|# _int]))
    (get-ffi-obj variable-name ncurses ctype)))

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
(define-ncurses mvwaddstr (_fun _window* _int _int _string -> _ok/err))
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
(define _attribute
  (let* ([_color #o00007700000]
         [_a (_bitmask (list 'normal     '= #o00000000000
                             'standout   '= #o00010000000
                             'underline  '= #o00020000000
                             'reverse    '= #o00040000000
                             'blink      '= #o00100000000
                             'dim        '= #o00200000000
                             'bold       '= #o00400000000
                             'altcharset '= #o01000000000
                             'attributes '= #o31777700000
                             'chartext   '= #o06000077777))]
         [r->c (ctype-scheme->c _a)]
         [c->r (ctype-c->scheme _a)])
    (make-ctype _uint
                (lambda [r] (let-values ([[color_pair attrs] (partition integer? r)])
                              (define attr (r->c attrs))
                              (cond [(null? color_pair) attr]
                                    ; ATTRIBUTES | COLOR_PAIR(N)
                                    [else (bitwise-ior attr (arithmetic-shift (car color_pair) 15))])))
                (lambda [c] (cons (arithmetic-shift (bitwise-and c _color) -15) (c->r c))))))

(define _color/term
  (let* ([_stdcolor (_enum '(none = -1 black red green blue yellow magenta cyan white) _short)]
         [r->c (ctype-scheme->c _stdcolor)]
         [c->r (ctype-c->scheme _stdcolor)])
    (make-ctype _short
                (lambda [r] (if (symbol? r) (r->c r) r))
                (lambda [c] (if (< c 16) (c->r c) c)))))

(define _rgb/component
  (make-ctype _short ;;; r/255 = c/1000 ==> 1000r = 255c
              (lambda [r] (exact-round (* r 1000/255))) 
              (lambda [c] (exact-round (* c 255/1000)))))

(define-ncurses wattron (_fun _window* _attribute -> _int -> #true))
(define-ncurses wattroff (_fun _window* _attribute -> _int -> #true))
(define-ncurses wattrset (_fun _window* _attribute -> _int -> #true))
(define-ncurses wstandout (_fun _window* -> _int -> 1))
(define-ncurses wstandend (_fun _window* -> _int -> 1))

(define-ncurses has_colors (_fun -> _bool))
(define-ncurses can_change_color (_fun -> _bool))
(define-ncurses use_default_colors (_fun -> _ok/err))
(define-ncurses assume_default_colors (_fun _color/term _color/term -> _ok/err))
(define-ncurses init_pair (_fun _short _color/term _color/term -> _ok/err))
(define-ncurses init_color (_fun _color/term _rgb/component _rgb/component _rgb/component -> _ok/err))

(define-ncurses pair_content (_fun [pair : _short]
                                   [fg : (_ptr o _color/term)]
                                   [bg : (_ptr o _color/term)]
                                   -> [ status : _ok/err]
                                   -> (and status (cons fg bg))))

(define-ncurses color_content (_fun [color : _color/term]
                                    [r/1000 : (_ptr o _rgb/component)]
                                    [g/1000 : (_ptr o _rgb/component)]
                                    [b/1000 : (_ptr o _rgb/component)]
                                    -> [status : _ok/err]
                                    -> (and status (list r/1000 g/1000 b/1000))))

;;; Miscellaneous
(define-ncurses def_prog_mode (_fun -> _int -> #true))
(define-ncurses reset_prog_mode (_fun -> _int -> #true))
(define-ncurses def_shell_mode (_fun -> _int -> #true))
(define-ncurses reset_shell_mode (_fun -> _int -> #true))



(module+ test
  (define stdscr (initscr))
  (void ((curry plumber-add-flush! (current-plumber))
         (lambda [this]
           (plumber-flush-handle-remove! this)
           (endwin))))

  (with-handlers ([exn? (lambda [e] (and (def_prog_mode) (endwin) (displayln e) (reset_prog_mode)))])
    (when (and stdscr (raw) (noecho) (idlok stdscr #true) (scrollok stdscr #true) (wtimeout stdscr -1))
      (when (has_colors)
        (start_color)
        (assume_default_colors 'none 'none))

      (init_pair 1 'blue 'none)
      (wattron stdscr (list 1 'blink))
      (for ([color (in-range (ncurs-extern-var 'COLORS))])
        (waddstr stdscr (format "~a~n" (color_content color)))
        (wrefresh stdscr))
      (wattroff stdscr (list 'blink)))

    (getch)))
