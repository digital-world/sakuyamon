""" These color definitions are used by izuna to highlight ncurses

" To see all colors execute ":XtermColorTable".
" xterm-256color only.

set background=dark
highlight clear
if exists("syntax_on")
    syntax reset
endif
let g:colors_name="gyoudmon"

"""" User Interface
" The normal Text, should be set before others
highlight Normal cterm=none ctermfg=none ctermbg=none
" The characters appear only when line wraps happen: '@' '-'
highlight NonText cterm=bold ctermfg=black ctermfg=16 
" The placeholder characters substituted for concealed text
highlight Conceal ctermfg=black
" Directory names or the items' label in the list
highlight Directory ctermfg=42 
" The error message on the command line
highlight ErrorMsg cterm=bold ctermfg=darkred ctermbg=234 
" The mode message: -- INSERT -- ...
highlight ModeMsg cterm=none ctermfg=red ctermfg=196 
" The message is given with the '-- More --'
highlight MoreMsg ctermfg=darkgreen 
" The warning message
highlight WarningMsg ctermfg=yellow ctermfg=226 
" The prompt message of command: 'y/q' ...
highlight Question ctermfg=green ctermfg=46 
" The status line of current window
highlight StatusLine cterm=underline ctermfg=242 
" The status line of non-current window
highlight StatusLineNC cterm=underline ctermfg=236 
" The line number
highlight LineNr ctermfg=214 
" The current match in 'wildmenu' completion
highlight WildMenu ctermfg=black ctermfg=16 ctermbg=darkcyan 
" Any titles for output from ':autocmd' or ':set all'
highlight Title ctermfg=grey ctermbg=blue
" The meta and special keyboard keys or
" the text used to show the unprintable characters 
highlight SpecialKey ctermfg=darkcyan 
" The column sparating vertically split window 
highlight VertSplit cterm=none ctermfg=238 
" The visual mode selection
highlight Visual cterm=reverse ctermfg=none ctermbg=none
" The visual mode selection 'NOT OWNED BY VIM'
highlight VisualNOS cterm=bold,underline

""" Code style
"" Single-element Groups
" Any comments
highlight Comment ctermfg=grey ctermfg=244 
" The text stand out the HTML link
highlight Underlined cterm=underline ctermfg=27 
" Any left blank and hidden
highlight Ignore cterm=bold ctermfg=235 ctermbg=black ctermbg=16
" Any erroneous costruct
highlight Error cterm=bold,reverse ctermfg=darkred ctermbg=234 
" Any tasks which need extra attention that marked in the comment
highlight Todo cterm=bold ctermfg=yellow ctermfg=226 ctermbg=none 

"" Constant
" Any constants 
highlight Constant ctermfg=red ctermfg=196 
" The string constant
highlight String ctermfg=202 
" The character constant: 'A' 'b' #\rubout
highlight Character ctermfg=214
" The number constants: 1987 0x00ff00 ...
highlight Number ctermfg=red ctermfg=196 
" Boolean constant: true FALSE #f ...
highlight Boolean ctermfg=208 
" The floating point constants: 6.67e-11
highlight Float ctermfg=red ctermfg=196

"" Identifier
" Any variable's name, also be used for the global functions
highlight Identifier cterm=none ctermfg=none ctermbg=none 
" The name of functions and methods of classes ...
highlight Function ctermfg=27

"" Statement
" Any statements
highlight Statement ctermfg=darkcyan 
" The condition-keywords: if switch endif ...
highlight Conditional ctermfg=darkcyan 
" The repeat-keywords: for each in ...
highlight Repeat ctermfg=grey ctermfg=51 
" Any labels: case default ...
highlight Label ctermfg=240 
" Any operators: 'new' '+' ...
highlight Operator ctermfg=208 
" Any other keywords: native assert ...
highlight Keyword ctermfg=42 
" The exception-keywords: throws try finally ...
highlight Exception ctermfg=220 

"" PreProc
" Any generic preprocessors
highlight PreProc ctermfg=48 
" The preprocessors for indicating the included sources
highlight Include ctermfg=213 
" Any preprocessors like '#define' in C/C++ language
highlight Define ctermfg=32
" Any preprocessors like '#define' in C/C++ language
highlight Macro ctermfg=38 
" Any condition-preprocessors: #if #elseif #endif ...
highlight PreCondit ctermfg=32 

"" Type
" The keywords represent primitive datatypes: int double ...
highlight Type ctermfg=green ctermfg=46 
" The keywords represent the accessibility of definitions: public internal ...
highlight NameSpace ctermfg=34 
" The keywords that represent the scope of definitions: static register ...
highlight StorageClass ctermfg=51 
" The keywords represent complex datatypes: enum union @interface ...
highlight Structure ctermfg=green ctermfg=46 
" The keywords represent meta datatypes: typedef data ...
highlight Typedef ctermfg=27

"" Special
" Any special symbols such as a regular expressions etc.
highlight Special ctermfg=black ctermfg=169 
" The special character within a constant
highlight SpecialChar ctermfg=208 
" Any tags which can use <C-]> on
highlight Tag ctermfg=208
" The character that needs attention: * ? ...
highlight Delimiter ctermfg=218
" The special things within the comment
highlight SpecialComment ctermfg=grey 
" Any debugging statement
highlight Debug ctermfg=darkgrey

""" Miscellaneous
"" Cursor
" The character under the cursor
highlight Cursor ctermfg=none ctermbg=none
" The character under the cursor [IME mode]
highlight CursorIM ctermfg=none ctermbg=none
" The column that the cursor is in if &cursorcolumn
highlight CursorColumn ctermfg=none ctermbg=234
" The line that the cursor is in if &cursorline
highlight CursorLine cterm=none ctermfg=none ctermbg=none
" Like LineNr when &cursorline/&relativenumber' is set for the cursor
highlight CursorLineNr cterm=bold ctermfg=202
" The columns set with &colorcolumn
highlight ColorColumn ctermfg=none ctermbg=233

"" Diff Mode
"The added line
highlight DiffAdd ctermfg=green ctermfg=46 ctermbg=240 
"The changed line
highlight DiffChange ctermfg=yellow ctermfg=226 ctermbg=240 
"The deleted line
highlight DiffDelete ctermfg=red ctermfg=196 ctermbg=240 
"The changed texts within the changed line
highlight DiffText ctermfg=grey ctermfg=51 ctermbg=240 

"" Popup Menu
" The normal items
highlight Pmenu ctermfg=yellow ctermfg=226 ctermbg=none 
" The scrollbar
highlight PmenuSbar cterm=none ctermfg=none ctermbg=none
" The selected item
highlight PmenuSel ctermfg=red ctermfg=196 ctermbg=none 
" The thumb of the scrollbar
highlight PmenuThumb ctermbg=166

"" Table Line
" Not active table page label
highlight TabLine ctermfg=red ctermfg=196 
" Where there are no labels
highlight TabLineFill ctermfg=grey ctermfg=244 ctermbg=blue ctermbg=21 
" Active table page label
highlight TabLineSel ctermfg=blue ctermfg=21 

"" User Action
" The last searched pattern or 
" the line in the quickfix window and some similiar items that need to stand out
highlight Search cterm=bold ctermfg=none ctermbg=26
" The texts that been searched or replaced by the '%s///c'
highlight IncSearch cterm=bold,underline ctermfg=magenta ctermbg=26
" The character of the paired bracket which under the cursor or just before it,
highlight MatchParen ctermfg=white ctermfg=231 
" The column with the specified width which indicates open and closed folds.
highlight FoldColumn ctermfg=81 ctermbg=none 
" The lines used for closed folds
highlight Folded ctermfg=223 ctermbg=none 
" The line where the signs displayed,
highlight SignColumn ctermfg=black ctermfg=16 ctermbg=white ctermbg=231
" the sign may be a breakpoint or an icon
highlight SignColor ctermfg=black ctermfg=16 ctermbg=white ctermbg=231

"" Spell
" The word that does not recognized by the spellchecker
highlight SpellBad ctermfg=darkred 
" The word should starts with a capital
highlight SpellCap ctermfg=green ctermfg=46 
" The word is recognized by the spellchecker and used in another region
highlight SpellLocal ctermfg=darkyellow 
" The word is recognized by the spellchecker and hardly ever used
highlight SpellRare ctermfg=yellow ctermfg=226 

