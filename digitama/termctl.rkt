#lang at-exp racket

(provide (except-out (all-defined-out) OK ERR))
(provide (struct-out chtype))

(require (for-syntax racket/syntax))

@require{digicore.rkt}
@require{posix.rkt}

(define-ffi-definer define-ncurses (ffi-lib "libncurses" #:global? #true))
(define-ffi-definer define-panel (ffi-lib "libpanel" #:global? #true))
(define-ffi-definer define-termctl (digimon-ffi-lib "termctl" #:global? #true))

(define OK (c-extern 'OKAY _int))
(define ERR (c-extern 'ERROR _int))

(define-cpointer-type _window*)
(define-cpointer-type _panel*)
(define _ok/err (make-ctype _int #false (lambda [c] (not (eq? c ERR)))))

(define _chtype ; you may work with vim highlight groups when FFIing to C
  (match-let* ([(list attrmask charmask cpairmask) (map (curryr c-extern _int) (list 'ATTRIBUTES 'CHARTEXT 'COLORPAIR))]
               [(list color_pair pair_number) (map c-extern (list 'color_pair 'pair_number) (list (_fun _short -> _int) (_fun _int -> _short)))]
               [_cterm (c-extern/bitmask (list 'NORMAL 'STANDOUT 'UNDERLINE 'UNDERCURL 'REVERSE 'INVERSE 'BLINK 'DIM 'BOLD 'INVIS 'PROTECT
                                               'HORIZONTAL 'LEFT 'LOW 'RIGHT 'TOP 'VERTICAL 'ALTCHARSET #| this is also used to tell wide char |#))]
               [chtype->c (lambda [_t] (bitwise-ior (bitwise-and (char->integer (chtype-char _t)) charmask)
                                                    ((ctype-scheme->c _cterm) (chtype-cterm _t))
                                                    (color_pair (chtype-ctermfg.bg _t))))])
    (make-ctype (ctype-basetype _cterm)
                (match-lambda ; racket->c
                  [(? chtype? chtype_t) (chtype->c chtype_t)]
                  [(? char? chartext-only) (bitwise-and (char->integer chartext-only) charmask)]
                  [(list-no-order (? integer? index) attrs ...) (bitwise-ior ((ctype-scheme->c _cterm) attrs) (color_pair index))]
                  [(list attributes-only ...) ((ctype-scheme->c _cterm) attributes-only)]
                  [(? symbol? r) (chtype->c (hash-ref :highlight r make-defchtype))]
                  [r (error '_chtype "no matching type for ~s" r)])
                (lambda [c] (chtype (integer->char (bitwise-and c charmask))
                                    ((ctype-c->scheme _cterm) (bitwise-and c attrmask))
                                    (pair_number (bitwise-and c cpairmask)))))))

(define _color-pair ; if working with vim highlight, only color-pair index will be used.
  (make-ctype _short
              (lambda [r] (if (symbol? r) (chtype-ctermfg.bg (hash-ref :highlight r make-defchtype)) r))
              values))

(define _color256
  (let ([_named-color ((lambda [c] (_enum c _short #:unknown values))
                       (foldl (lambda [c Cs] (list* c '= (hash-ref vim-colors (symbol->string c)) Cs)) (list 'none '= -1)
                              ;;; racket->c can map multi names to one value, while c->racket uses the last name
                              ;;; names in aliases will not be the value of c->racket
                              (let ([aliases (list 'lightred 'lightgreen 'lightyellow 'lightblue 'lightmagenta 'lightcyan
                                                   'darkgray 'lightgray 'lightgrey 'gray 'brown)])
                                (reverse (append aliases (remove* aliases (map string->symbol (hash-keys vim-colors))))))))])
    (make-ctype _short
                (lambda [r] (let ([name (case r [(fg foreground) (unbox normal-ctermfg)] [(bg background) (unbox normal-ctermfg)] [else r])])
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
                             [(? parameter? untyped-racket-needs-it) (untyped-racket-needs-it bar)]
                             [(? procedure? hook/2-as-expected) (hook/2-as-expected bar cols)]
                             [(list-no-order (? procedure? hook/2) attrs ...) (void (wbkgdset bar attrs) (hook/2 bar cols))]
                             [(? symbol? name-of-prefab-window) (hash-set! :prefabwindows name-of-prefab-window bar)
                              #| TODO: check if the wrapped deallocator will release the storage as expected |#])))))))
        -> _ok/err ; always #true
        -> (void) #| Racket should not output this value |#))

(define-ncurses initscr (_fun -> [curwin : _window*/null] -> (and (hash-set! :prefabwindows 'stdscr curwin) curwin)))
(define-ncurses beep (_fun -> _ok/err))
(define-ncurses flash (_fun -> _ok/err))
(define-ncurses raw (_fun -> _ok/err)) ; this one is better than (cbreak), it stops all user signals.
(define-ncurses noraw (_fun -> _ok/err))
(define-ncurses noecho (_fun -> _ok/err))
(define-ncurses start_color (_fun -> _ok/err))
(define-ncurses flushinp (_fun -> _ok/err -> #true))
(define-ncurses qiflush (_fun -> _ok/err)) ; Break, SIGINT, SIGQUIT
(define-ncurses intrflush (_fun [null : _window*/null = #false] _bool -> _ok/err)) ; SIGINT, SIGQUIT, SIGTSTP
(define-ncurses keypad (_fun [stdwin : _window* = (:prefabwin 'stdscr)] _bool -> _ok/err))
(define-ncurses idlok (_fun _window* _bool -> _ok/err))
(define-ncurses idcok (_fun _window* _bool -> _ok/err))
(define-ncurses scrollok (_fun _window* _bool -> _ok/err))
(define-ncurses clearok (_fun _window* _bool -> _ok/err))
(define-ncurses delwin (_fun _window* -> _ok/err) #:wrap (deallocator))  ;;; deallocator always return void
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
(define-ncurses newpad (_fun [nrows : _int] [ncols : _int] -> _window*/null) #:wrap (allocator delwin))
(define-ncurses newwin (_fun [nrows : _int] [ncols : _int] [y : _int] [x : _int]
                             -> [stdwin : _window*/null]
                             -> (and stdwin (wbkgdset stdwin (make-defchtype)) stdwin)) #:wrap (allocator delwin))

(define-ncurses-winapi border (_fun ; please use (wattr_on) and friends to highlight border lines.
                               [_chtype = (:altchar 'VLINE)] [_chtype = (:altchar 'VLINE)] [_chtype = (:altchar 'HLINE)] [_chtype = (:altchar 'HLINE)]
                               [_chtype = (:altchar 'ULCORNER)] [_chtype = (:altchar 'URCORNER)] [_chtype = (:altchar 'LLCORNER)] [_chtype = (:altchar 'LRCORNER)]
                               -> _ok/err))
(define-ncurses-winapi/mv vline (_fun [sym : _chtype = (:altchar 'VLINE)] [maxlong : _int] -> _ok/err))
(define-ncurses-winapi/mv hline (_fun [sym : _chtype = (:altchar 'HLINE)] [maxlong : _int] -> _ok/err))

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

(define-ncurses-winapi clear (_fun -> _ok/err)) ;;; it just calls (erase), also do (move 0 0)
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
                                         -> (and $? (set-box! normal-ctermfg 'none) (set-box! normal-ctermbg 'none) $?)))
(define-ncurses assume_default_colors (_fun [fg : _color256] [bg : _color256]
                                            -> [$? : _ok/err]
                                            -> (and $? (set-box! normal-ctermfg fg) (set-box! normal-ctermbg bg) $?)))

(define-ncurses-winapi bkgd (_fun _chtype -> _void))
(define-ncurses-winapi bkgdset (_fun _chtype -> _void))
(define-ncurses-winapi attron (_fun _chtype -> _ok/err)) ; macro of attr_on
(define-ncurses-winapi attrset (_fun _chtype -> _ok/err)) ; macro of attr_set
(define-ncurses-winapi attroff (_fun _chtype -> _ok/err)) ; macro of attr_off
(define-ncurses-winapi standout (_fun -> _ok/err)) ; macro of (attrset (list 'standout))
(define-ncurses-winapi standend (_fun -> _ok/err)) ; macro of (attrset (list 'normal))
(define-ncurses-winapi attr_on (_fun [attr : _chtype] [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi attr_set (_fun [attr : _chtype] [pair_index : _color-pair] [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi attr_get (_fun [As : (_ptr o _chtype)] [cp : (_ptr o _color-pair)] [opts : _pointer = #false] -> _ok/err -> (values As cp)))
(define-ncurses-winapi attr_off (_fun [attr : _chtype] [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi color_set (_fun [pair_index : _color-pair] [opts : _pointer = #false] -> _ok/err))
(define-ncurses-winapi/mv chgat (_fun [howmany : _int] [attrs : _chtype] [pair_index : _color-pair] [opts : _pointer = #false] -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one is a curse, even we have 256 * 256 pair slots, however we cannot use ;;;
;;; them more than 256 - 1 (the first one is reserved as default), because pair   ;;;
;;; No. is combined with attribute and color, the actual storage is only 8bit.    ;;;
;;; It's your duty to manage the palette.                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses init_pair (_fun _color-pair _color256 _color256 -> _ok/err))
(define-ncurses pair_content (_fun [pair : _color-pair] [fg : (_ptr o _color256)] [bg : (_ptr o _color256)]
                                   -> [$? : _ok/err]
                                   -> (and $? (cons fg bg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one is not recommended because ncurses cannot restore the correct color  ;;;
;;; value after (endwin). All changed colors keep taking effects until you        ;;;
;;; restart your terminal.                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses init_color (_fun _color256 _rgb/component _rgb/component _rgb/component -> _ok/err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one isn't reliable, ncurses just gets palette information from terminfo. ;;;
;;; Now the problem is: 256 colors do work perfectly, but I don't know its RGBs.  ;;;
;;; Again, if colors are changed by application, it might pollute the terminal.   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses color_content
  (_fun [color : _color256]
        [r/1000 : (_ptr o _rgb/component)] [g/1000 : (_ptr o _rgb/component)] [b/1000 : (_ptr o _rgb/component)]
        -> [$? : _ok/err]
        -> (and $? (list r/1000 g/1000 b/1000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; So let this one always return false.                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ncurses can_change_color (_fun -> _bool -> #false))

;;; Panels
(define-panel del_panel (_fun _panel* -> _ok/err) #:wrap (deallocator))
(define-panel new_panel (_fun _window* -> _panel*/null) #:wrap (allocator del_panel))
(define-panel update_panels (_fun -> _void)) ; followed by (doupdate).
(define-panel top_panel (_fun _panel* -> _ok/err))
(define-panel bottom_panel (_fun _panel* -> _ok/err))
(define-panel show_panel (_fun _panel* -> _ok/err))
(define-panel hide_panel (_fun _panel* -> _ok/err))
(define-panel panel_hidden (_fun _panel* -> _bool))
(define-panel move_panel (_fun _panel* _int _int -> _ok/err))
(define-panel panel_above (_fun _panel* -> _panel*))
(define-panel panel_below (_fun _panel* -> _panel*))
(define-panel set_panel_userptr (_fun _panel* _racket -> _ok/err))
(define-panel panel_userptr (_fun _panel* -> _racket))
(define-panel panel_window (_fun _panel* -> _window*))
(define-panel replace_panel (_fun _panel* _window* -> _window*))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define :altchar
  (let* ([acsmap (c-extern 'initailizer_element_should_be_constant (_fun _symbol -> _chtype))]
         [acs-map (delay #| at this point ncureses has not initailized yet |#
                    (make-hash (map (lambda [acs] (cons acs (acsmap acs)))
                                    (list 'ULCORNER 'LLCORNER 'URCORNER 'LRCORNER 'RTEE 'LTEE 'BTEE 'TTEE 'HLINE 'VLINE
                                          'S1 'S3 'S7 'S9 'DIAMOND 'CKBOARD 'DEGREE 'BULLET 'BOARD 'LANTERN 'BLOCK 'STERLING
                                          'LARROW 'RARROW 'DARROW 'UARROW 'PLUS 'PLMINUS 'LEQUAL 'GEQUAL 'PI 'NEQUAL))))])
    (lambda [key #:extra-attrs [attrs null]] ; if working with vim highlight, then only cterm will be used. 
      (define altchar (hash-ref (force acs-map) key make-defchtype))
      (chtype (chtype-char altchar) (append (chtype-cterm altchar) attrs) 0))))

(define :prefabwin
  (lambda [name]
    (hash-ref :prefabwindows name
              (lambda [] (error ':prefabwin "no such previously fabricated window: ~a" name)))))

(define :colorscheme! ;;; this function should invoked after (initscr)
  (lambda [colors.vim #:exn-handler [uncaught-exn void]]
    #| What's different from vim is that the Normal group is always used as default setting |#
    (when (file-exists? colors.vim)
      (define token->symbol (compose1 string->symbol string-downcase))
      (define token->number (compose1 (curry min 256) string->number))
      (define token->symlist (lambda [t] (remove* (list 'none) (map token->symbol (string-split t #px"(=|,)+")))))
      (hash-clear! :highlight)
      (use_default_colors)
      (with-input-from-file colors.vim
        (thunk (for ([line (in-port read-line)]
                     #:when (< (hash-count :highlight) (sub1 256))
                     #:when (regexp-match? #px"c?term([fb]g)?=" line))
                 (define-values [attrs head] (partition (curry regexp-match #px"=") (string-split line)))
                 (define ipair (add1 (hash-count :highlight)))
                 (define hl (vector ipair null (unbox normal-ctermfg) (unbox normal-ctermbg)))
                 (define groupname (string->symbol (last head)))
                 (for ([token (in-list attrs)])
                   (match token
                     [(pregexp #px"c?term=(.+)" (list _ attrs)) (vector-set! hl 1 (token->symlist attrs))]
                     [(pregexp #px"ctermfg=(\\d+)" (list _ nfg)) (vector-set! hl 2 (token->number nfg))]
                     [(pregexp #px"ctermfg=(\\D+)" (list _ fg)) (vector-set! hl 2 (token->symbol fg))]
                     [(pregexp #px"ctermbg=(\\d+)" (list _ nbg)) (vector-set! hl 3 (token->number nbg))]
                     [(pregexp #px"ctermbg=(\\D+)" (list _ bg)) (vector-set! hl 3 (token->symbol bg))]
                     [_ (void '(skip gui settings))]))
                 (with-handlers ([exn? uncaught-exn])
                   (cond [(symbol=? groupname 'Normal) (assume_default_colors (vector-ref hl 2) (vector-ref hl 3))]
                         [else (void (init_pair ipair (vector-ref hl 2) (vector-ref hl 3))
                                     (hash-set! :highlight groupname (chtype #\nul (vector-ref hl 1) (vector-ref hl 0))))]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))

  (require (submod "posix.rkt" typed/ffi))
  (require (for-syntax racket/syntax))

  (require/typed/provide/pointers Window* Panel*)
  (require/typed/provide/ctypes _chtype _color-pair _color256 _rgb/component _keycode)

  (define-type CTerm (Listof Symbol))
  (define-type ChType chtype)
  (define-type CharType (U ChType Char (Pairof Byte CTerm) CTerm Symbol))
  (define-type Color-Pair (U Byte Symbol))
  (define-type Color256 (U Byte Symbol))
  (define-type RGB/Component Byte)
  (define-type KeyCode (U False Symbol Char))
  (define-type Ripoffline-StdInit (Window* Positive-Integer -> Any))
  (define-type Ripoffline-Init (U Symbol (Parameterof Window*/Null) Ripoffline-StdInit (Pairof Ripoffline-StdInit CTerm)))

  (define-syntax (define-type-ncurses-winapi stx)
    (syntax-case stx [->]
      [[_ id (dom0 anti-dead-loop ... -> rng)]
       #'(define-type-ncurses-winapi id (-> dom0 anti-dead-loop ... rng))]
      [[_ id (-> dom/rng ...)]
       (with-syntax ([wid (format-id #'id "w~a" (syntax-e #'id))])
         #'(require/typed/provide (submod "..")
                                  [id (-> dom/rng ...)]
                                  [wid (-> Window* dom/rng ...)]))]))

  (define-syntax (define-type-ncurses-winapi/mv stx)
    (syntax-case stx [->]
      [[_ id (dom0 anti-dead-loop ... -> rng)]
       #'(define-type-ncurses-winapi/mv id (-> dom0 anti-dead-loop ... rng))]
      [[_ id (-> dom/rng ...)]
       (with-syntax ([mvid (format-id #'id "mv~a" (syntax-e #'id))]
                     [mvwid (format-id #'id "mvw~a" (syntax-e #'id))])
         #'(begin (define-type-ncurses-winapi id (-> dom/rng ...))
                  (require/typed/provide (submod "..")
                                         [mvid (-> Natural Natural dom/rng ...)]
                                         [mvwid (-> Window* Natural Natural dom/rng ...)])))]))

  (require/typed/provide (submod "..")
                         [#:struct chtype ([char : Char]
                                           [cterm : CTerm]
                                           [ctermfg.bg : Byte])]
                         [:altchar (-> Symbol [#:extra-attrs CTerm] ChType)]
                         [:prefabwin (-> Symbol Window*)])
  
  (require/typed/provide (submod "..")
                         [ripoffline (-> Integer Ripoffline-Init Void)]
                         [initscr (-> Window*/Null)]
                         [beep (-> Boolean)]
                         [flash (-> Boolean)]
                         [raw (-> Boolean)]
                         [noraw (-> Boolean)]
                         [noecho (-> Boolean)]
                         [start_color (-> Boolean)]
                         [flushinp (-> True)]
                         [qiflush (-> Boolean)]
                         [intrflush (Boolean -> Boolean)]
                         [keypad (Boolean -> Boolean)]
                         [idlok (Window* Boolean -> Boolean)]
                         [idcok (Window* Boolean -> Boolean)]
                         [scrollok (Window* Boolean -> Boolean)]
                         [clearok (Window* Boolean -> Boolean)]
                         [delwin (Window* -> Void)]
                         [endwin (-> Boolean)]
                         [newpad (Positive-Integer Positive-Integer -> Window*/Null)]
                         [newwin (Natural Natural Natural Natural -> Window*/Null)]
                         [mvwin (Window* Natural Natural -> Boolean)]
                         [getcury (Window* -> Natural)]
                         [getcurx (Window* -> Natural)]
                         [getbegy (Window* -> Natural)]
                         [getbegx (Window* -> Natural)]
                         [getmaxy (Window* -> Positive-Integer)]
                         [getmaxx (Window* -> Positive-Integer)]
                         [putwin (Path-String -> Boolean)]
                         [getwin (Path-String -> Boolean)]
                         [overlay (Window* Window* -> Boolean)]
                         [overwrite (Window* Window* -> Boolean)]
                         [copywin (-> Window* Window* Natural Natural Natural Natural Natural Natural Boolean Boolean)]
                         [prefresh (-> Window* Natural Natural Natural Natural Natural Natural Boolean)]
                         [pnoutrefresh (-> Window* Natural Natural Natural Natural Natural Natural Boolean)]
                         [wresize (Window* Positive-Integer Positive-Integer -> Boolean)]
                         [wnoutrefresh (Window* -> Boolean)]
                         [doupdate (-> Boolean)]
                         [has_colors (-> Boolean)]
                         [use_default_colors (-> Boolean)]
                         [assume_default_colors (Color256 Color256 -> Boolean)]
                         [init_pair (Color-Pair Color256 Color256 -> Boolean)]
                         [pair_content (Color-Pair -> (Option (Pairof Color256 Color256)))]
                         [init_color (Color256 RGB/Component RGB/Component RGB/Component -> Boolean)]
                         [color_content (Color256 -> (Option (List RGB/Component RGB/Component RGB/Component)))]
                         [can_change_color (-> False)]
                         [curs_set (Integer -> Integer)]
                         [def_prog_mode (-> True)]
                         [reset_prog_mode (-> True)]
                         [def_shell_mode (-> True)]
                         [reset_shell_mode (-> True)]
                         [scr_dump (Path-String -> Boolean)]
                         [scr_restore (Path-String -> Boolean)]
                         [scr_init (Path-String -> Boolean)]
                         [scr_set (Path-String -> Boolean)])

  (require/typed/provide (submod "..")
                         [del_panel (Panel* -> Boolean)]
                         [new_panel (Window* -> Panel*)]
                         [update_panels (-> Void)]
                         [top_panel (Panel* -> Boolean)]
                         [bottom_panel (Panel* -> Boolean)]
                         [show_panel (Panel* -> Boolean)]
                         [hide_panel (Panel* -> Boolean)]
                         [panel_hidden (Panel* -> Boolean)]
                         [move_panel (Panel* Natural Natural -> Boolean)]
                         [panel_above (Panel* -> Panel*)]
                         [panel_below (Panel* -> Panel*)]
                         [set_panel_userptr (Panel* Any -> Boolean)]
                         [panel_userptr (Panel* -> Any)]
                         [panel_window (Panel* -> Window*)]
                         [replace_panel (Panel* Window* -> Window*)])
  
  (require/typed/provide (submod "..")
                         [:colorscheme! (-> Path-String [#:exn-handler (-> exn Any)] Void)])

  (define-type-ncurses-winapi border (-> Boolean))
  (define-type-ncurses-winapi timeout (Integer -> Boolean))
  (define-type-ncurses-winapi clear (-> Boolean))
  (define-type-ncurses-winapi clrtobot (-> Boolean))
  (define-type-ncurses-winapi clrtoeol (-> Boolean))
  (define-type-ncurses-winapi insdelln (Integer -> Boolean))
  (define-type-ncurses-winapi deleteln (-> Boolean))
  (define-type-ncurses-winapi insertln (-> Boolean))
  (define-type-ncurses-winapi move (Natural Natural -> Boolean))
  (define-type-ncurses-winapi scrl (Integer -> Boolean))
  (define-type-ncurses-winapi refresh (-> Boolean))
  (define-type-ncurses-winapi setscrreg (Natural Natural -> Boolean))
  (define-type-ncurses-winapi bkgd (CharType -> Void))
  (define-type-ncurses-winapi bkgdset (CharType -> Void))
  (define-type-ncurses-winapi attron (CharType -> Boolean))
  (define-type-ncurses-winapi attrset (CharType -> Boolean))
  (define-type-ncurses-winapi attroff (CharType -> Boolean))
  (define-type-ncurses-winapi standout (-> Boolean))
  (define-type-ncurses-winapi standend (-> Boolean))
  (define-type-ncurses-winapi attr_on (CharType -> Boolean))
  (define-type-ncurses-winapi attr_set (CharType Color-Pair -> Boolean))
  (define-type-ncurses-winapi attr_get (-> (values ChType Color-Pair)))
  (define-type-ncurses-winapi attr_off (CharType -> Boolean))
  (define-type-ncurses-winapi color_set (Color-Pair -> Boolean))

  (define-type-ncurses-winapi/mv vline (Natural -> Boolean))
  (define-type-ncurses-winapi/mv hline (Natural -> Boolean))
  (define-type-ncurses-winapi/mv getch (-> KeyCode))
  (define-type-ncurses-winapi/mv inch (-> ChType))
  (define-type-ncurses-winapi/mv insch (-> Boolean))
  (define-type-ncurses-winapi/mv delch (-> Boolean))
  (define-type-ncurses-winapi/mv addch (CharType -> Boolean))
  (define-type-ncurses-winapi/mv addstr (String -> Boolean))
  (define-type-ncurses-winapi/mv addwstr (String -> Boolean))
  (define-type-ncurses-winapi/mv chgat (Natural CharType Color-Pair -> Boolean)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* main typed/racket
  (require "digicore.rkt")
  (require (submod ".." typed/ffi))

  (require/typed racket
                 [#:opaque Plumber plumber?]
                 [#:opaque Plumber-Flush-Handle plumber-flush-handle?]
                 [current-plumber (Parameterof Plumber)]
                 [plumber-add-flush! (-> Plumber (-> Plumber-Flush-Handle Any) Plumber-Flush-Handle)]
                 [plumber-flush-handle-remove! (-> Plumber-Flush-Handle Void)]
                 [srcloc->string (-> srcloc String)])
  
  (require/typed (submod ".." digitama)
                 [:highlight (HashTable Symbol ChType)])
  
  (ripoffline +1 (list (lambda [titlebar cols] (waddwstr titlebar (~a "> racket digitama/termctl.rkt" #:width cols))) 'reverse))
  (ripoffline -1 'statusbar)
  (void (initscr)
        (plumber-add-flush! (current-plumber)
                            (lambda [this] (plumber-flush-handle-remove! this) (endwin))))
  
  (define uncaught-exn : (-> exn KeyCode)
    (lambda [errobj]
      (define messages ((inst call-with-input-string (Listof String)) (exn->string errobj) port->lines))
      (clear)
      (addstr (format "»» name: ~a~n" (object-name errobj)))
      (unless (null? messages)
        (define msghead " message: ")
        (define msgspace (~a #:min-width (max 0 (sub1 (string-length msghead)))))
        (addstr (format "»»~a~a~n" msghead (car messages)))
        (for-each (lambda [msg] (addstr (format "»»»~a~a~n" msgspace msg))) (cdr messages)))
      (refresh)
      (getch)))
  
  (with-handlers ([exn:fail? (compose1 void uncaught-exn)])
    (unless (and (has_colors) (start_color) (use_default_colors) (curs_set 0)
                 (raw) (noecho) (timeout -1) (keypad #true))
      (error "A thunk of initializing failed!")))

  (define :hitest : (-> Path-String String KeyCode)
    (lambda [colors.vim hints]
      (with-handlers ([exn:fail? uncaught-exn])
        (:colorscheme! colors.vim #:exn-handler uncaught-exn)
        (define-values [units fields] (values (+ 24 (apply max 0 (map (compose1 string-length symbol->string) (hash-keys :highlight)))) 3))
        (define-values [cols rows] (values (* fields units) (max (ceiling (/ (hash-count :highlight) fields)) 1)))
        (define stdscr (:prefabwin 'stdscr))
        (define statusbar (:prefabwin 'statusbar))
        (define stdclr (newwin 0 0 0 0)) ; full screen window
        (define padclr (newpad rows cols))
        
        (unless (and (window*? stdclr) (window*? padclr))
          (error "Unable to create color area!"))
        
        (for ([name (in-list (sort (hash-keys :highlight) symbol<?))])
          (wattrset padclr name)
          (define-values [attr colorpair] (wattr_get padclr))
          (waddstr padclr (~a name (pair_content colorpair) #:width units)))
        (wstandend padclr)
    
        (let demostrate : KeyCode ()
          (define-values [maxy mby y] (let ([maxy (getmaxy stdscr)]) (values maxy (min maxy (+ rows 2)) (max (quotient (- maxy rows 2) 2) 0))))
          (define-values [maxx mbx x] (let ([maxx (getmaxx stdscr)]) (values maxx (min maxx (+ cols 2)) (max (quotient (- maxx cols 2) 2) 0))))
          (define-values [title title-offset] (values (format " ~a " (file-name-from-path colors.vim)) 2))
          (wresize stdclr mby mbx)
          (mvwin stdclr y x)
          (when (false? padclr) (error "Unable to create color area border!"))
          (for-each wclear (list stdscr statusbar))
          (wattrset stdclr 'VertSplit)
          (wborder stdclr)
          (unless (< mbx (+ title-offset (string-length title) 2))
            (wmove stdclr 0 2)
            (mvwaddch stdclr 0 title-offset (:altchar 'RTEE))
            (mvwaddch stdclr 0 (+ title-offset (string-length title) 1) (:altchar 'LTEE))
            (wattrset stdclr 'TabLineFill)
            (mvwaddstr stdclr 0 (add1 title-offset) title))
          (wattrset statusbar 'StatusLine)
          (mvwaddstr statusbar 0 0 (~a hints #:width maxx))
          (for-each wnoutrefresh (list stdscr stdclr statusbar))
          (pnoutrefresh padclr 0 0 (add1 y) (add1 x) (min (+ rows y 1) (sub1 maxy)) (min (+ cols x 1) (sub1 maxx)))
          (doupdate)

          (let ([ch (getch)])
            (if (eq? ch 'SIGWINCH)
                (demostrate)
                (for-each delwin (list stdclr padclr)))
            ch)))))

  (match (current-command-line-arguments)
    [(vector) (:hitest (build-path (digimon-stone) "colors.vim") "Press any key to Exit!")]
    [colors (let :colorscheme ([index 0])
              (case (:hitest (vector-ref colors index) "Control Hints: [J: Next; K: Prev; Others: Exit]")
                [(#\J #\j) (:colorscheme (min (add1 index) (sub1 (vector-length colors))))]
                [(#\K #\k) (:colorscheme (max (sub1 index) 0))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module digitama racket
  (provide (all-defined-out))

  (struct chtype (char cterm ctermfg.bg) #:prefab)
  (define normal-ctermfg (box 'none))
  (define normal-ctermbg (box 'none))
  (define :highlight (make-hasheq))
  (define :prefabwindows (make-hasheq))
  (define make-defchtype (thunk (chtype #\nul null 0))))

(require (submod "." digitama))
