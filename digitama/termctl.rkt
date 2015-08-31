#lang at-exp racket

(provide (except-out (all-defined-out) OK ERR))
(provide (all-from-out "posix.rkt"))

(require (for-syntax racket/syntax))

@require{posix.rkt}

(define-ffi-definer define-ncurses (ffi-lib "libncurses" #:global? #true))
(define-ffi-definer define-termctl
  (ffi-lib (build-path (digimon-digitama) (car (use-compiled-file-paths)) "native" (system-library-subpath #false) "termctl")
           #:global? #true))

(define OK (c-extern 'OKAY _int))
(define ERR (c-extern 'ERROR _int))

(define _window* (_cpointer/null 'WINDOW*))
(define _ok/err (make-ctype _int #false (lambda [c] (not (eq? c ERR)))))
(define stdscr (make-parameter #false))

(define _attr ; you may work with vim highlight groups (attributes only) when FFIing to C
  (let ([_cterm ((lambda [a] (_bitmask a _int))
                 (foldl (lambda [a As] (list* (string->symbol (string-downcase (symbol->string a))) '= (c-extern a _uint) As))
                        null (list 'NORMAL 'STANDOUT 'UNDERLINE 'UNDERCURL 'REVERSE 'INVERSE 'BLINK 'DIM 'BOLD 'INVIS 'PROTECT
                                   'HORIZONTAL 'LEFT 'LOW 'RIGHT 'TOP 'VERTICAL 'ALTCHARSET)))])
    (make-ctype (ctype-basetype _cterm)
                (lambda [r] ((ctype-scheme->c _cterm) (if (symbol? r) (highlight-cterm (hash-ref vim-highlight r (const defgroup))) r)))
                (ctype-c->scheme _cterm))))

(define _color-pair ; you may work with vim highlight groups (color-pair index only) when FFIing to C
  (make-ctype _short
              (lambda [r] (if (symbol? r) (highlight-index (hash-ref vim-highlight r (const defgroup))) r))
              values))

(define _chtype ; you may work with vim highlight groups when FFIing to C
  (let ([attrmask (c-extern 'ATTRIBUTES _int)]
        [charmask (c-extern 'CHARTEXT _int)]
        [cpairmask (c-extern 'COLORPAIR _int)]
        [color_pair (c-extern 'color_pair (_fun _short -> _int))]
        [pair_number (c-extern 'pair_number (_fun _int -> _short))])
    (make-ctype (ctype-basetype _attr)
                (lambda [r] (let ([chtype->integer (lambda [ch_t] (bitwise-ior (bitwise-and (char->integer (chtype-char ch_t)) charmask)
                                                                               ((ctype-scheme->c _attr) (chtype-attributes ch_t))
                                                                               (color_pair (chtype-color-pair ch_t))))])
                              (cond [(chtype? r) (chtype->integer r)]
                                    [(char? r) (bitwise-and (char->integer r) charmask)]
                                    [(integer? r) (bitwise-and r charmask)]
                                    [(and (symbol? r) (hash-ref vim-highlight r (const defgroup)))
                                     => (lambda [hi] (chtype->integer (chtype #\nul (highlight-cterm hi) (highlight-index hi))))]
                                    [else r])))
                (lambda [c] (chtype (integer->char (bitwise-and c charmask))
                                    ((ctype-c->scheme _attr) (bitwise-and c attrmask))
                                    (pair_number (bitwise-and c cpairmask)))))))

(define _color-256
  (let ([_named-color ((lambda [c] (_enum c _short #:unknown values))
                       (foldl (lambda [c Cs] (list* c '= (hash-ref vim-colors (symbol->string c)) Cs)) (list 'none '= -1)
                              ;;; racket->c can map multi names to one value, while c->racket uses the last name
                              ;;; names in aliases will not be the value of c->racket
                              (let ([aliases (list 'lightred 'lightgreen 'lightyellow 'lightblue 'lightmagenta 'lightcyan
                                                   'darkgray 'lightgray 'lightgrey 'gray 'brown)])
                                (reverse (append aliases (remove* aliases (map string->symbol (hash-keys vim-colors))))))))])
    (make-ctype _short
                (lambda [r] (let ([name (case r [(fg foreground) (unbox normal-foreground)] [(bg background) (unbox normal-background)] [else r])])
                              (if (integer? name) name ((ctype-scheme->c _named-color) name))))
                (lambda [c] (if (<= -1 c 15) ((ctype-c->scheme _named-color) c) c)))))

(define _rgb/component
  (make-ctype _short ;;; r/255 = c/1000 <==> 1000r = 255c
              (lambda [r] (exact-round (* r 1000/255))) 
              (lambda [c] (exact-round (* c 255/1000)))))

(define _keycode
  (make-ctype _int #false
              (lambda [c] (with-handlers ([exn? (const #false)])
                            ((curryr hash-ref c (thunk (integer->char c)))
                             (hasheq #x0003 'SIGINT #x001C 'SIGQUIT #x019A 'SIGWINCH))))))

(define acs-map (let ([acsmap (c-extern 'initailizer_element_should_be_constant (_fun _symbol -> _chtype))])
                  (delay #| at this point ncureses has not initailized yet |#
                    (make-hash (map (lambda [acs] (cons acs (acsmap acs)))
                                    (list 'ULCORNER 'LLCORNER 'URCORNER 'LRCORNER 'RTEE 'LTEE 'BTEE 'TTEE 'HLINE 'VLINE
                                          'S1 'S3 'S7 'S9 'DIAMOND 'CKBOARD 'DEGREE 'BULLET 'BOARD 'LANTERN 'BLOCK 'STERLING
                                          'LARROW 'RARROW 'DARROW 'UARROW 'PLUS 'PLMINUS 'LEQUAL 'GEQUAL 'PI 'NEQUAL))))))

(define acs_map
  (lambda [key #:extra_attrs [attrs null]]
    (define altchar (hash-ref (force acs-map) key (const defchar)))
    (define extra (if (symbol? attrs) (highlight-cterm (hash-ref vim-highlight attrs (const defgroup))) attrs))
    (chtype (chtype-char altchar) (append (chtype-attributes altchar) extra) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         WARNING: ncurses uses YX-Coordinate System                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-ncurses-winapi stx)
  (syntax-case stx [_fun]
    [[_ id (_fun IDL-spec ...)]
     (with-syntax ([wid (format-id #'id "w~a" (syntax-e #'id))])
       #'(begin (define-ncurses id (_fun IDL-spec ...))
                (define-ncurses wid (_fun [stdwin : _window*] IDL-spec ...))))]))

(define-syntax (define-ncurses-winapi/mv stx)
  (syntax-case stx [_fun]
    [[_ id (_fun IDL-spec ...)]
     (with-syntax ([mvid (format-id #'id "mv~a" (syntax-e #'id))]
                   [mvwid (format-id #'id "mvw~a" (syntax-e #'id))])
       #'(begin (define-ncurses-winapi id (_fun IDL-spec ...))
                (define-ncurses mvid (_fun [y : _int] [x : _int] IDL-spec ...))
                (define-ncurses mvwid (_fun [stdwin : _window*] [y : _int] [x : _int] IDL-spec ...))))]))

;;; Initialization
(define-ncurses ripoffline ; this can be invoked up to 5 times
  (_fun [gt0@top/lt0@bottom : _int]
        ((lambda [r->c] (make-ctype _fpointer r->c #false))
         (lambda [r] ((curryr function-ptr (_fun [bar : _window*] [cols : _int] -> _int))
                      (lambda [bar cols] ; (ripoffline) registers (hook/2) to be invoked inside (initscr)
                        (unless (false? bar) ; inability to allocate the ripped off line window
                          ((lambda _ (if (wnoutrefresh bar) OK ERR)) ; (hook/2) should return an integer value which is indeed useless
                           (match r
                             [(? parameter? outside-needs-this) (outside-needs-this bar)]
                             [(? procedure? hook/2-as-expected) (hook/2-as-expected bar cols)]
                             [(list-no-order (? procedure? hook/2) attrs ...) (void (wbkgdset bar (chtype #\nul attrs 0)) (hook/2 bar cols))])))))))
        -> _ok/err ; always #true
        -> (void)))

(define-ncurses initscr (_fun -> [curwin : _window*] -> (and (stdscr curwin) curwin)))
(define-ncurses beep (_fun -> _ok/err))
(define-ncurses flash (_fun -> _ok/err))
(define-ncurses raw (_fun -> _ok/err))
(define-ncurses cbreak (_fun -> _ok/err))
(define-ncurses noecho (_fun -> _ok/err))
(define-ncurses start_color (_fun -> _ok/err))
(define-ncurses flushinp (_fun -> _ok/err -> #true))
(define-ncurses qiflush (_fun -> _ok/err)) ; Break, SIGINT, SIGQUIT
(define-ncurses intrflush (_fun [null : _window* = #false] _bool -> _ok/err)) ; SIGINT, SIGQUIT, SIGTSTP
(define-ncurses keypad (_fun [stdwin : _window* = (stdscr)] _bool -> _ok/err))
(define-ncurses idlok (_fun _window* _bool -> _ok/err))
(define-ncurses idcok (_fun _window* _bool -> _ok/err))
(define-ncurses scrollok (_fun _window* _bool -> _ok/err))
(define-ncurses clearok (_fun _window* _bool -> _ok/err))
(define-ncurses delwin (_fun _window* -> _ok/err) #:wrap (deallocator))
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
;;;  its size cannot be changed manually.                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses newpad (_fun [nrows : _int] [ncols : _int] -> _window*) #:wrap (allocator delwin))
(define-ncurses newwin (_fun [nrows : _int] [ncols : _int] [y : _int] [x : _int]
                             -> [stdwin : _window*]
                             -> (and stdwin (wbkgdset stdwin defchar) stdwin)) #:wrap (allocator delwin))

(define-ncurses-winapi border (_fun ; please use (wattr_on) and friends to highlight border lines.
                               [_chtype = (acs_map 'VLINE)] [_chtype = (acs_map 'VLINE)] [_chtype = (acs_map 'HLINE)] [_chtype = (acs_map 'HLINE)]
                               [_chtype = (acs_map 'ULCORNER)] [_chtype = (acs_map 'URCORNER)] [_chtype = (acs_map 'LLCORNER)] [_chtype = (acs_map 'LRCORNER)]
                               -> _ok/err))
(define-ncurses-winapi/mv vline (_fun [sym : _chtype = (acs_map 'VLINE)] [maxlong : _int] -> _ok/err))
(define-ncurses-winapi/mv hline (_fun [sym : _chtype = (acs_map 'HLINE)] [maxlong : _int] -> _ok/err))

(define-ncurses mvwin (_fun _window* [y : _int] [x : _int] -> _ok/err))
(define-ncurses getcury (_fun _window* -> _int))
(define-ncurses getcurx (_fun _window* -> _int))
(define-ncurses getbegy (_fun _window* -> _int))
(define-ncurses getbegx (_fun _window* -> _int))
(define-ncurses getmaxy (_fun _window* -> _int))
(define-ncurses getmaxx (_fun _window* -> _int))

;;; TODO: add support to winchstr, winstr and friends which can get content from buffer.
(define-ncurses-winapi timeout (_fun _int -> _ok/err))
(define-ncurses-winapi/mv getch (_fun -> _keycode))
(define-ncurses-winapi/mv inch (_fun -> _chtype))

(define-ncurses-winapi clear (_fun -> _ok/err)) ;;; it just calls (*erase)
(define-ncurses-winapi clrtobot (_fun -> _ok/err))
(define-ncurses-winapi clrtoeol (_fun -> _ok/err))
(define-ncurses-winapi insdelln (_fun _int -> _ok/err))
(define-ncurses-winapi deleteln (_fun -> _ok/err))
(define-ncurses-winapi insertln (_fun -> _ok/err))
(define-ncurses-winapi/mv insch (_fun -> _ok/err))
(define-ncurses-winapi/mv delch (_fun -> _ok/err))
(define-ncurses-winapi/mv addch (_fun _chtype -> _ok/err))
(define-ncurses-winapi/mv addstr (_fun _string -> _ok/err))
(define-ncurses-winapi/mv addwstr (_fun _string/ucs-4 -> _ok/err))

(define-ncurses putwin (_fun _path -> _ok/err))
(define-ncurses getwin (_fun _path -> _ok/err))
(define-ncurses overlay (_fun [src : _window*] [dest : _window*] -> _ok/err)) ; blanks are not copied
(define-ncurses overwrite (_fun [src : _window*] [dest : _window*] -> _ok/err)) ; blanks are copied either
(define-ncurses copywin (_fun [src : _window*] [dest : _window*] [srcy : _int] [srcx : _int]
                              [desty : _int] [destx : _int] [desty+height : _int] [destx+width : _int] [overlay : _bool]
                               -> _ok/err))

; Negative coordinates in these two rontines are treated as zero.
(define-ncurses prefresh (_fun [pad : _window*] [pady : _int] [padx : _int]
                               [screeny : _int] [screenx : _int] [screeny+height : _int] [screenx+width : _int]
                               -> _ok/err))
(define-ncurses pnoutrefresh (_fun [src : _window*] [pady : _int] [padx : _int]
                                   [screeny : _int] [screenx : _int] [screeny+height : _int] [screenx+width : _int]
                                   -> _ok/err))

(define-ncurses-winapi move (_fun _int _int -> _ok/err))
(define-ncurses-winapi scrl (_fun _int -> _ok/err))
(define-ncurses-winapi refresh (_fun -> _ok/err))
(define-ncurses-winapi setscrreg (_fun [top : _int] [bot : _int] -> _ok/err))
(define-ncurses wresize (_fun _window* [rows : _int] [cols : _int] -> _ok/err))
(define-ncurses wnoutrefresh (_fun _window* -> _ok/err))
(define-ncurses doupdate (_fun -> _ok/err))

;;; Color and Attribute functions
(define-ncurses has_colors (_fun -> _bool))
(define-ncurses use_default_colors (_fun -> [$? : _ok/err]
                                         -> (and $? (set-box! normal-foreground 'none) (set-box! normal-background 'none) $?)))
(define-ncurses assume_default_colors (_fun [fg : _color-256] [bg : _color-256]
                                            -> [$? : _ok/err]
                                            -> (and $? (set-box! normal-foreground fg) (set-box! normal-background bg) $?)))

(define-ncurses-winapi bkgd (_fun _chtype -> _void))
(define-ncurses-winapi bkgdset (_fun _chtype -> _void))
(define-ncurses-winapi/mv chgat (_fun [howmany : _int] [attrs : _attr] [pair_index : _color-pair] [opts : _pointer = #false] -> _ok/err))

(define-ncurses-winapi attron (_fun _chtype -> _ok/err))
(define-ncurses-winapi attrset (_fun _chtype -> _ok/err))
(define-ncurses-winapi attroff (_fun _chtype -> _ok/err))
(define-ncurses-winapi attr_on (_fun _attr [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi attr_set (_fun _attr _color-pair [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi attr_get (_fun [As : (_ptr o _attr)] [cp : (_ptr o _color-pair)] [opts : _pointer = #false] -> _ok/err -> (values As cp)))
(define-ncurses-winapi attr_off (_fun _attr [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi color_set (_fun _color-pair [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi standout (_fun -> _ok/err))
(define-ncurses-winapi standend (_fun -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one is a curse, even we have 256 * 256 pair slots, however we cannot use ;;;
;;; them more than 256 - 1 (the first one is reserved as default), because pair   ;;;
;;; No. is combined with attribute and color, the actual storage is only 8bit.    ;;;
;;; It's your duty to manage the palette.                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses init_pair (_fun _short _color-256 _color-256 -> _ok/err))
(define-ncurses pair_content (_fun [pair : _short] [fg : (_ptr o _color-256)] [bg : (_ptr o _color-256)]
                                   -> [$? : _ok/err]
                                   -> (and $? (cons fg bg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one is not recommended because ncurses cannot restore the correct color  ;;;
;;; value after (endwin). All changed colors keep taking effects until you        ;;;
;;; restart your terminal.                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses init_color (_fun _color-256 _rgb/component _rgb/component _rgb/component -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one isn't reliable, ncurses just gets palette information from terminfo. ;;;
;;; Now the problem is: 256 colors do work perfectly, but I don't know its RGBs.  ;;;
;;; Again, if colors are changed by application, it might pollute the terminal.   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses color_content
  (_fun [color : _color-256]
        [r/1000 : (_ptr o _rgb/component)] [g/1000 : (_ptr o _rgb/component)] [b/1000 : (_ptr o _rgb/component)]
        -> [$? : _ok/err]
        -> (and $? (list r/1000 g/1000 b/1000))))

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
(define load-vim-highlight! ;;; this function should invoked after (initscr)
  (lambda [colors.vim #:exn-handler [uncaught-exn void]]
    #| What's different from vim is that the Normal group is always used as default setting |#
    (when (file-exists? colors.vim)
      (define token->symbol (compose1 string->symbol string-downcase))
      (define token->number (compose1 (curry min 256) string->number))
      (define token->symlist (lambda [t] (remove* (list 'none) (map token->symbol (string-split t #px"(=|,)+")))))
      (hash-clear! vim-highlight)
      (use_default_colors)
      (with-input-from-file colors.vim
        (thunk (for ([line (in-port read-line)]
                     #:when (< (hash-count vim-highlight) (sub1 256))
                     #:when (regexp-match? #px"c?term([fb]g)?=" line))
                 (define-values [attrs head] (partition (curry regexp-match #px"=") (string-split line)))
                 (define ipair (add1 (hash-count vim-highlight)))
                 (define hi (vector ipair null (unbox normal-foreground) (unbox normal-background)))
                 (define groupname (string->symbol (last head)))
                 (for ([token (in-list attrs)])
                   (match token
                     [(pregexp #px"c?term=(.+)" (list _ attrs)) (vector-set! hi 1 (token->symlist attrs))]
                     [(pregexp #px"ctermfg=(\\d+)" (list _ nfg)) (vector-set! hi 2 (token->number nfg))]
                     [(pregexp #px"ctermfg=(\\D+)" (list _ fg)) (vector-set! hi 2 (token->symbol fg))]
                     [(pregexp #px"ctermbg=(\\d+)" (list _ nbg)) (vector-set! hi 3 (token->number nbg))]
                     [(pregexp #px"ctermbg=(\\D+)" (list _ bg)) (vector-set! hi 3 (token->symbol bg))]
                     [_ (void '(skip gui settings))]))
                 (with-handlers ([exn? uncaught-exn])
                   (cond [(symbol=? groupname 'Normal) (assume_default_colors (vector-ref hi 2) (vector-ref hi 3))]
                         [else (void (init_pair ipair (vector-ref hi 2) (vector-ref hi 3))
                                     (hash-set! vim-highlight groupname (apply highlight (vector->list hi))))]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define statusbar (make-parameter #false))
  (ripoffline 1 (list 'reverse (lambda [titlebar cols] (waddwstr titlebar (~a "> racket digitama/termctl.rkt" #:width cols)))))
  (ripoffline -1 statusbar)
  (void (initscr)
        (plumber-add-flush! (current-plumber) (lambda [this] (plumber-flush-handle-remove! this) (endwin))))

  (define (uncaught-exn errobj)
    (define messages (call-with-input-string (exn-message errobj) port->lines))
    (clear)
    (addstr (format "»» name: ~a~n" (object-name errobj)))
    (unless (null? messages)
      (define msghead " message: ")
      (define msgspace (~a #:min-width (sub1 (string-length msghead))))
      (addstr (format "»»~a~a~n" msghead (car messages)))
      (for-each (lambda [msg] (addstr (format "»»»~a~a~n" msgspace msg))) (cdr messages)))
    (for ([stack (in-list (continuation-mark-set->context (exn-continuation-marks errobj)))])
      (when (cdr stack)
        (define srcinfo (srcloc->string (cdr stack)))
        (unless (or (false? srcinfo) (regexp-match? #px"^/" srcinfo))
          (addstr (format "»»»» ~a: ~a~n" srcinfo (or (car stack) 'λ))))))
    (refresh)
    (getch))
  
  (with-handlers ([exn:fail? (compose1 void uncaught-exn)])
    (unless (and (stdscr) (statusbar) (has_colors) (start_color) (use_default_colors) (curs_set 0)
                 (raw) (noecho) (timeout -1) (keypad #true))
      (error "A thunk of initializing failed!")))

  (define :highlight
    (lambda [colors.vim hints]
      (with-handlers ([exn:fail? uncaught-exn])
        (load-vim-highlight! colors.vim #:exn-handler uncaught-exn)
        (define-values [units fields] (values (+ 24 (apply max 0 (map (compose1 string-length symbol->string) (hash-keys vim-highlight)))) 3))
        (define-values [cols rows] (values (* fields units) (max (ceiling (/ (hash-count vim-highlight) fields)) 1)))
        (define stdclr (newwin 0 0 0 0)) ; full screen window
        (define colorscheme (newpad rows cols))
        (when (false? colorscheme) (error "Unable to create color area!"))
        (for ([name (in-list (sort (hash-keys vim-highlight) symbol<?))])
          (wattrset colorscheme name)
          (define-values [attr colorpair] (wattr_get colorscheme))
          (waddstr colorscheme (~a name (pair_content colorpair) #:width units)))
        (wstandend colorscheme)
    
        (let display-colors ()
          (define-values [maxy mby y] (let ([maxy (getmaxy (stdscr))]) (values maxy (min maxy (+ rows 2)) (max (quotient (- maxy rows 2) 2) 0))))
          (define-values [maxx mbx x] (let ([maxx (getmaxx (stdscr))]) (values maxx (min maxx (+ cols 2)) (max (quotient (- maxx cols 2) 2) 0))))
          (define-values [title title-offset] (values (format " ~a " (file-name-from-path colors.vim)) 2))
          (wresize stdclr mby mbx)
          (mvwin stdclr y x)
          (when (false? colorscheme) (error "Unable to create color area border!"))
          (for-each wclear (list (stdscr) (statusbar)))
          (wattrset stdclr 'VertSplit)
          (wborder stdclr)
          (unless (< mbx (+ title-offset (string-length title) 2))
            (wmove stdclr 0 2)
            (mvwaddch stdclr 0 title-offset (acs_map 'RTEE))
            (mvwaddch stdclr 0 (+ title-offset (string-length title) 1) (acs_map 'LTEE))
            (wattrset stdclr 'TabLineFile)
            (mvwaddstr stdclr 0 (add1 title-offset) title))
          (wattrset (statusbar) 'StatusLine)
          (mvwaddstr (statusbar) 0 0 (~a hints #:width maxx))
          (for-each wnoutrefresh (list (stdscr) stdclr (statusbar)))
          (pnoutrefresh colorscheme 0 0 (add1 y) (add1 x) (min (+ rows y 1) (sub1 maxy)) (min (+ cols x 1) (sub1 maxx)))
          (doupdate)

          (let ([ch (getch)])
            (if (eq? ch 'SIGWINCH)
                (display-colors)
                (for-each delwin (list stdclr colorscheme)))
            ch)))))

  (match (current-command-line-arguments)
    [(vector) (:highlight (build-path (digimon-stone) "colors.vim") "Press any key to Exit!")]
    [colors (let :colorscheme ([index 0])
              (case (:highlight (vector-ref colors index) "Control Hints: [J: Next; K: Prev; Others: Exit]")
                [(#\J #\j) (:colorscheme (min (add1 index) (sub1 (vector-length colors))))]
                [(#\K #\k) (:colorscheme (max (sub1 index) 0))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama racket
  (provide (all-defined-out))

  (struct chtype (char attributes color-pair) #:prefab)
  (define defchar (chtype #\nul null 0))
  
  (struct highlight (index cterm ctermfg ctermbg) #:prefab)
  (define vim-highlight (make-hasheq))
  (define defgroup (highlight 0 null 'foreground 'background))
  (define normal-foreground (box 'none))
  (define normal-background (box 'none)))

(require (submod "." digitama))
