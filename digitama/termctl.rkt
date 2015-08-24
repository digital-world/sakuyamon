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
(define _ok/err (make-ctype _int #false (lambda [c] (not (eq? c -1)))))

(define _chtype
  (let ([mask (c-extern 'CHARTEXT)])
    ;;; although char is combined with attribute and color, use (wcolor_set) and (wattr_on) instead
    (make-ctype _uint
                (lambda [r] (bitwise-and (char->integer r) mask))
                (lambda [c] (integer->char (bitwise-and c mask))))))

(define _attr
  ((lambda [a] (_bitmask a _uint))
   (foldl (lambda [a As] (list* (string->symbol (string-downcase (symbol->string a))) '= (c-extern a #:ctype _uint) As))
          null (list 'NORMAL 'STANDOUT 'UNDERLINE 'REVERSE 'BLINK 'DIM 'BOLD 'INVIS 'PROTECT))))

(define _color/term
  (let* ([_named-color ((lambda [c] (_enum c _short #:unknown values))
                        (foldl (lambda [c Cs] (list* c '= (hash-ref vim-colors (symbol->string c)) Cs)) (list 'none '= -1)
                               ;;; racket->c can map multi names to one value, while c->racket uses the last name
                               ;;; names in aliases will not be the value of c->racket
                               (let ([aliases (list 'lightred 'lightgreen 'lightyellow 'lightblue 'lightmagenta 'lightcyan
                                                    'darkgray 'lightgray 'lightgrey 'gray 'brown)])
                                 (reverse (append aliases (remove* aliases (map string->symbol (hash-keys vim-colors))))))))])
    (make-ctype _short
                (lambda [r] (if (integer? r) r ((ctype-scheme->c _named-color) r)))
                (ctype-c->scheme _named-color))))

(define _rgb/component
  (make-ctype _short ;;; r/255 = c/1000 <==> 1000r = 255c
              (lambda [r] (exact-round (* r 1000/255))) 
              (lambda [c] (exact-round (* c 255/1000)))))

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
(define-ncurses delwin (_fun _window* -> _ok/err))
(define-ncurses endwin (_fun -> _ok/err))

;;; Windows/Pads and Input/Output functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For the sake of simplicity, I only deal with pads.                            ;;;
;;;                                                                               ;;;
;;; Shrinking terminal will cause windows resizing propertionlessly, while after  ;;;
;;;  that reenlarging terminal will cause windows resizing in propertion to the   ;;;
;;;  changed but wrong size.                                                      ;;;
;;; The subwindow and subpad suck                                                 ;;;
;;;  it must be deleted first when deleting its parent;                           ;;;
;;;  scrolling is unavaliable (as well as pads);                                  ;;;
;;;  it will not save you from the move-and-repainting headache.                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses newpad (_fun [nlines : _int] [ncols : _int] -> _window*))
(define-ncurses mvwvline (_fun _window* [beginy : _int] [beginx : _int] [sym : _chtype = #\|] [maxlong : _int] -> _ok/err))
(define-ncurses mvwhline (_fun _window* [beginy : _int] [beginx : _int] [sym : _chtype = #\-] [maxlong : _int] -> _ok/err))

(define-ncurses getcury (_fun _window* -> _int))
(define-ncurses getcurx (_fun _window* -> _int))
(define-ncurses getmaxy (_fun _window* -> _int))
(define-ncurses getmaxx (_fun _window* -> _int))

(define-ncurses putwin (_fun _path -> _ok/err))
(define-ncurses getwin (_fun _path -> _ok/err))
(define-ncurses wclear (_fun _window* -> _ok/err)) ;;; it just calls (werase)
(define-ncurses wclrtobot (_fun _window* -> _ok/err))
(define-ncurses wclrtoeol (_fun _window* -> _ok/err))

(define-ncurses wtimeout (_fun _window* _int -> _ok/err)) ;;; blocking mode for getch()
(define-ncurses wgetch (_fun _window* -> [key : _int] -> (cond [(negative? key) #false] [else (integer->char key)])))
(define-ncurses wmove (_fun _window* _int _int -> _ok/err))
(define-ncurses waddwstr (_fun _window* _string/ucs-4 -> _ok/err))
(define-ncurses mvwaddwstr (_fun _window* _int _int _string/ucs-4 -> _ok/err))
(define-ncurses wrefresh (_fun _window* -> _ok/err))
(define-ncurses wnoutrefresh (_fun _window* -> _ok/err))
(define-ncurses doupdate (_fun -> _ok/err))

(define-ncurses prefresh (_fun [pad : _window*] [pady : _int] [padx : _int]
                               [screeny : _int] [screenx : _int] [screeny+height : _int] [screenx+width : _bool]
                               -> _ok/err))
(define-ncurses pnoutrefresh (_fun [src : _window*] [pady : _int] [padx : _int]
                                   [screeny : _int] [screenx : _int] [screeny+height : _int] [screenx+width : _bool]
                                   -> _ok/err))

;;; Color and Attribute functions
(define-ncurses has_colors (_fun -> _bool))
(define-ncurses use_default_colors (_fun -> _ok/err))
(define-ncurses assume_default_colors (_fun _color/term _color/term -> _ok/err))

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
(define-ncurses curs_set (_fun _int -> _int))
(define-ncurses def_prog_mode (_fun -> _int -> #true))
(define-ncurses reset_prog_mode (_fun -> _int -> #true))
(define-ncurses def_shell_mode (_fun -> _int -> #true))
(define-ncurses reset_shell_mode (_fun -> _int -> #true))

(define-ncurses scr_dump (_fun _file -> _ok/err))
(define-ncurses scr_restore (_fun _file -> _ok/err))
(define-ncurses scr_init (_fun _file -> _ok/err))
(define-ncurses scr_set (_fun _file -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct color-pair (index cterm ctermfg ctermbg) #:prefab)
(define :syntax (curryr hash-ref (const (color-pair 0 null 'none 'none))))
(define vim-highlight->ncurses-color-pair
  (lambda [colors.vim]
    (define highlight (make-hash))
    (when (file-exists? colors.vim)
      (with-input-from-file colors.vim
        (thunk (for ([line (in-port read-line)]
                     #:when (< (hash-count highlight) 256)
                     #:when (regexp-match? #px"c?term([fb]g)?=" line))
                 (define-values [attrs head] (partition (curry regexp-match #px"=") (string-split line)))
                 (match-define (list cterm ctermfg ctermbg) (map make-parameter (list null 'none 'none)))
                 (for ([token (in-list attrs)])
                   (match token
                     [(pregexp #px"ctermfg=(\\d+)" (list _ nfg)) (ctermfg (min 256 (string->number nfg)))]
                     [(pregexp #px"ctermfg=(\\D+)" (list _ fg)) (ctermfg (string->symbol (string-downcase fg)))]
                     [(pregexp #px"ctermbg=(\\d+)" (list _ nbg)) (ctermbg (min 256 (string->number nbg)))]
                     [(pregexp #px"ctermbg=(\\D+)" (list _ bg)) (ctermbg (string->symbol (string-downcase bg)))]
                     [term (cterm (remove 'none (map (compose1 string->symbol string-downcase) (cdr (string-split term #px"[=,]")))))]))
                 (hash-set! highlight (string->symbol (last head)) (color-pair (add1 (hash-count highlight)) (cterm) (ctermfg) (ctermbg)))))))
    highlight))

(define wstandon
  (lambda [stdwin info]
    (when (< 0 (color-pair-index info))
      (init_pair (color-pair-index info) (color-pair-ctermfg info) (color-pair-ctermbg info)))
    (wattr_on stdwin (color-pair-cterm info))
    (wcolor_set stdwin (color-pair-index info))))

(define mvwaddhistr
  (lambda [stdwin y x info fmt . contexts]
    (wstandon stdwin info)
    (mvwaddwstr stdwin y x (apply format fmt contexts))
    (wstandend stdwin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define stdscr (initscr))
  (define atexit (plumber-add-flush! (current-plumber) (lambda [this] (plumber-flush-handle-remove! this) (endwin))))

  (define (uncaught-exn e)
    (for ([line (in-list (call-with-input-string (exn-message e) port->lines))])
      (waddwstr stdscr (~a line #\newline #:min-width (getmaxx stdscr))))
    (wrefresh stdscr)
    (void (wgetch stdscr)))

  (with-handlers ([exn:fail? uncaught-exn])
    (unless (and (wrefresh stdscr) (has_colors) (start_color) (use_default_colors) (curs_set 0)
                 (raw) (noecho) (wtimeout stdscr -1) (keypad stdscr #true) (idlok stdscr #true))
      (error "A thunk of initializing failed!"))
    
    (define colors.vim (build-path (digimon-stone) "colors.vim"))
    (define highlight (vim-highlight->ncurses-color-pair colors.vim))
    (define-values [units fields] (values (+ 12 (apply max (map (compose1 string-length symbol->string) (hash-keys highlight)))) 4))
    (define-values [cols rows] (values (add1 (* fields units)) (+ 2 (ceiling (/ (hash-count highlight) fields)))))
    (define colorscheme (newpad rows cols))
    (when (false? colorscheme) (error "Unable to create color area!"))
    
    (let display-colors ()
      (define y (max (quotient (- (getmaxy stdscr) rows) 2) 0))
      (define x (max (quotient (- (getmaxx stdscr) cols) 2) 0))
      (wclear stdscr)
      (mvwaddhistr stdscr 0 0 (:syntax highlight 'Visual) (~a "> raco test digivice/sakuyamon/izuna.rkt" #:min-width (getmaxx stdscr)))
      (wmove colorscheme 0 0)
      (for ([[name group] (in-hash highlight)]
            [index (in-naturals 1)])
        (mvwaddhistr stdscr (getcury colorscheme) (getcurx colorscheme) group
                     (~a name #\[ (+ y (getcury colorscheme)) #\, (+ x (getcurx colorscheme)) #\] #:min-width units))
        (mvwaddhistr colorscheme (getcury colorscheme) (getcurx colorscheme) group
                     (~a name #\[ (color-pair-ctermfg group) #\, (color-pair-ctermbg group) #\] #:min-width units))
        (when (zero? (remainder index fields)) (waddwstr colorscheme (~a #\newline))))
      (mvwaddhistr stdscr (- (getmaxy stdscr) 2) 0 (:syntax highlight 'StatusLine) (~a "Press any key to exit!" #:min-width (getmaxx stdscr)))
      (wnoutrefresh stdscr)
      (pnoutrefresh colorscheme 0 0 y x (+ y rows) (+ x cols))
      (doupdate)
      
      (when (char=? (wgetch colorscheme) #\u19A)
        (display-colors)))))
