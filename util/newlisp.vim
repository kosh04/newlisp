" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <cyril@slobin.pp.ru>
" URL:          http://www.vim.org/scripts/script.php?script_id=2067
" Another URL:  http://slobin.pp.ru/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th Anniversary)
" Last change:  2010 May 09 (The Great Victory 65th Anniversary)
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.31 2010/05/10 00:45:56 slobin Exp $

" *** Some syntax quirks of newLISP and how this file treats them: ***
"
" * In the following text, the word "character" refers to the ASCII
"   characters, and the word "symbol" to the lisp symbols (= atoms).
"
" * All built-in symbols are in the same class (Statement); functions,
"   definitions, macros and control structures aren't distinguished.
"
" * Special syntax for : character (colon) is underway, meantime it is
"   just highlighted and not checked otherwise.
"
" * Quoting character ' (apostrophe) is allowed anywhere, even before
"   closing parenthesis. Interpreter uses it this way, this file just
"   does the same.
"
" * newLISP interpreter doesn't insist that numbers must be separated
"   from the following numbers or symbols. E.g. the sequence 1.2.3e4e5
"   is valid and equal to three tokens 1.2 .3e4 e5. Such a monster is
"   rather a typo than someones intent, so it is marked as an error.
"
" * If a backlash character \ in a quoted string is not followed by
"   a valid escape sequence, the interpreter doesn't alarm you, it
"   just silently removes this character from the string. E.g. the
"   string "a\b" is equal to the string "ab". Again, this is rather
"   a typo than someones intent, so it is marked as an error.
"
" * Brackets [ ] and braces { } are used by newLISP as the alternate
"   string delimiters. Although, when they doesn't fit into delimiter
"   syntax, they treated by interpreter as the ordinary characters.
"   E.g. {string} is a string while string} is symbol, or [text] is
"   a string delimiter while text] is a symbol. Such a symbols are
"   rather typos than someones intent, so they are marked as errors.
"
" * You can disable any kind of highlighting by putting an appropriate
"   "hi link" line into your .vimrc. Some examples:
"
"   To disable the highlighting of the nested left parentheses in the
"   first column as an error:
"
"     hi link newlispLeftParenError newlispParenthesis
"
"   To disable the highlighting of the special symbols as special:
"
"     hi link newlispSymbolSpecial newlispSymbol
"
" * This syntax file is not compatible with Vim version 5.x and elder.
"   Where have you dug them out? It should work with Vim 6.x, although
"   this was not heavily tested. I use Vim 7.2 for development.
"
" * New in this version: multiple patches by LM adopted, got ready to
"   upcoming newLISP 10.3 release

if exists("b:current_syntax")
  finish
endif

syntax case match

setlocal iskeyword=33,36-38,42,43,45-47,48-57,60-64,@,92,94,_,124,126

syn region newlispComment oneline start="[;#]" end="$" contains=newlispTodo,@Spell
syn keyword newlispTodo FIXME TODO XXX contained

syn region newlispDocComment start="^;;\(\s\|$\)" end="^\(;;\(\s\|$\)\)\@!" contains=newlispTodo,newlispDocExample,newlispDocUserKeyword,newlispDocKeyword,newlispDocItalic,newlispDocMonospace,newlispDocHTMLTag,newlispDocHTMLEntity,@Spell
syn region newlispDocExample matchgroup=newlispDocKeyword start="^;;\s@example$"ms=s+3 matchgroup=NONE end="^\(;;\(\s\|$\)\)\@!" contained
syn match newlispDocUserKeyword "^;;\s@[a-zA-Z_-]*\s"ms=s+3,me=e-1 contained
syn match newlispDocKeyword "^;;\s@\(module\|index\|description\|location\|version\|author\|syntax\|param\|return\)\s"ms=s+3,me=e-1 contained
syn match newlispDocKeyword "@link\s"me=e-1 contained
syn match newlispDocItalic "<[^<>]\+>"hs=s+1,he=e-1 contained
syn match newlispDocMonospace "'[^']\+'"hs=s+1,he=e-1 contained
syn match newlispDocHTMLTag "<\/\=\(h1\|h2\|h3\|h4\|i\|em\|b\|tt\|p\|pre\|center\|li\|ul\|blockquote\)>" contained
syn match newlispDocHTMLTag "<\(br\|hr\)\/\=>" contained
syn match newlispDocHTMLEntity "&\w\+;" contained

syn cluster newlispListContent contains=TOP,newlispRightParenError
syn region newlispList matchgroup=newlispParenthesis start="(" end=")" contains=@newlispListContent,newlispListError
syn region newlispListError matchgroup=newlispLeftParenError start="^(" matchgroup=newlispParenthesis end=")" contains=@newlispListContent,newlispListError contained
syn match newlispRightParenError ")"

syn match newlispSymbol "\<\([+-]\=\d\)\@!\k\+\>"
syn match newlispSymbolSpecial "\<\([+-]\=\d\)\@!\k*\W\>"
syn match newlispSymbolSpecial "\<\u\k*\>"

syn match newlispQuote "'" nextgroup=newlispQuote,newlispQuotedSymbol skipwhite
syn match newlispQuotedSymbol "\<\([+-]\=\d\)\@!\k\+\>" contained

syn match newlispNumberError "\<[+-]\=\d\k\+\>"
syn match newlispNumberDec "\<[+-]\=[1-9]\d*\>"
syn match newlispNumberOct "\<[+-]\=0\o*\>"
syn match newlispNumberHex "\<[+-]\=0[Xx]\x\+\>"

syn match newlispFloat "\<[+-]\=\d\+[Ee][+-]\=\d\+\>"
syn match newlispFloat "\<[+-]\=\.\d\+\([Ee][+-]\=\d\+\)\=\>"
syn match newlispFloat "\<[+-]\=\d\+\.\d*\([Ee][+-]\=\d\+\)\=\>"

syn region newlispStringQuoted start=+"+ skip=+\\"+ end=+"+ contains=newlispEscapeError,newlispEscape
syn match newlispEscapeError +\\+ contained
syn match newlispEscape +\\["\\nrt]+ contained
syn match newlispEscape +\\\d\d\d+ contained
syn match newlispEscape +\\x\x\x+ contained

syn match newlispBracketError "[][}{]"

syn region newlispStringBraced start="{" end="}" contains=newlispStringBraced
syn region newlispStringTexted start="\[text\]" end="\[\/text\]"

" This keywords list is based on newLISP v.10.2.6 build primes.h file
" XXX Don't forget to remove colon ":" and escape vertical bar "|"

syn keyword newlispFunction ! != $ % & * + ++ - -- / < << <= = > >= >> NaN? ^ abort abs acos acosh
syn keyword newlispFunction add address amb and append append-file apply args array array-list
syn keyword newlispFunction array? asin asinh assoc atan atan2 atanh atom? base64-dec base64-enc
syn keyword newlispFunction bayes-query bayes-train begin beta betai bind binomial bits callback
syn keyword newlispFunction case catch ceil change-dir char chop clean close command-event cond cons
syn keyword newlispFunction constant context context? continue copy copy-file cos cosh count cpymem
syn keyword newlispFunction crc32 crit-chi2 crit-z current-line curry date date-value debug dec
syn keyword newlispFunction def-new default define define-macro delete delete-file delete-url
syn keyword newlispFunction destroy det device difference directory directory? div do-until do-while
syn keyword newlispFunction doargs dolist dostring dotimes dotree dump dump-symbol dup empty?
syn keyword newlispFunction encrypt ends-with env erf error-event error-number error-text eval
syn keyword newlispFunction eval-string exec exists exit exp expand explode extend factor fft
syn keyword newlispFunction file-info file? filter find find-all first flat float float? floor flt
syn keyword newlispFunction for for-all fork format fv gammai gammaln gcd get-char get-float get-int
syn keyword newlispFunction get-long get-string get-url global global? if if-not ifft import inc
syn keyword newlispFunction index inf? int integer integer? intersect invert irr join lambda? last
syn keyword newlispFunction last-error legal? length let letex letn list list? load local log lookup
syn keyword newlispFunction lower-case macro? main-args make-dir map mat match max member min mod
syn keyword newlispFunction mul multiply net-accept net-close net-connect net-error net-eval
syn keyword newlispFunction net-interface net-listen net-local net-lookup net-packet net-peek
syn keyword newlispFunction net-peer net-ping net-receive net-receive-from net-receive-udp
syn keyword newlispFunction net-select net-send net-send-to net-send-udp net-service net-sessions
syn keyword newlispFunction new nil? normal not now nper npv nth null? number? open or pack parse
syn keyword newlispFunction parse-date peek pipe pmt pop pop-assoc post-url pow prefix pretty-print
syn keyword newlispFunction primitive? print println prob-chi2 prob-z process prompt-event
syn keyword newlispFunction protected? push put-url pv quote quote? rand random randomize read
syn keyword newlispFunction read-buffer read-char read-expr read-file read-key read-line read-utf8
syn keyword newlispFunction reader-event real-path receive ref ref-all regex regex-comp remove-dir
syn keyword newlispFunction rename-file replace reset rest reverse rotate round save search seed
syn keyword newlispFunction seek select self semaphore send sequence series set set-locale set-ref
syn keyword newlispFunction set-ref-all setf setq sgn share signal silent sin sinh sleep slice sort
syn keyword newlispFunction source spawn sqrt starts-with string string? sub swap sym symbol?
syn keyword newlispFunction symbols sync sys-error sys-info tan tanh term throw throw-error time
syn keyword newlispFunction time-of-day timer title-case trace trace-highlight transpose trim true?
syn keyword newlispFunction unicode unify unique unless unpack until upper-case utf8 utf8len uuid
syn keyword newlispFunction wait-pid when while write write-buffer write-char write-file write-line
syn keyword newlispFunction xfer-event xml-error xml-parse xml-type-tags zero? \| ~

syn keyword newlispVariable ostype $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $it $main-args
syn keyword newlispKeyword fn lambda
syn keyword newlispPreloaded Class Tree module

syn keyword newlispObsolete assoc-set nth-set ref-set replace-assoc set-assoc set-nth name

" "continue" is defined under #ifdef, not compiled in by default
syn keyword newlispDebugging continue

" "integer" is still defined in 10.2.6
syn keyword newlispObsolete integer

syn match newlispColon ":"
syn match newlispComma ","

syn keyword newlispBoolean nil true

hi def link newlispNumberDec newlispNumber
hi def link newlispNumberOct newlispNumber
hi def link newlispNumberHex newlispNumber

hi def link newlispStringQuoted newlispString
hi def link newlispStringBraced newlispString
hi def link newlispStringTexted newlispString

hi def link newlispLeftParenError newlispError
hi def link newlispRightParenError newlispError
hi def link newlispNumberError newlispError
hi def link newlispEscapeError newlispError
hi def link newlispBracketError newlispError

hi def link newlispComment Comment
hi def link newlispTodo Todo
hi def link newlispDocComment Comment
hi def link newlispDocExample Comment
hi def link newlispDocUserKeyword Identifier
hi def link newlispDocKeyword Type
hi def link newlispDocItalic CommentItalic
hi def link newlispDocMonospace CommentUnderlined
hi def link newlispDocHTMLTag Statement
hi def link newlispDocHTMLEntity Special
hi def link newlispList Normal
hi def link newlispParenthesis Delimiter
hi def link newlispSymbol Identifier
hi def link newlispSymbolSpecial Type
hi def link newlispQuote Type
hi def link newlispQuotedSymbol Type
hi def link newlispNumber Number
hi def link newlispFloat Float
hi def link newlispString String
hi def link newlispEscape Special
hi def link newlispFunction Statement
hi def link newlispVariable Statement
hi def link newlispKeyword Statement
hi def link newlispPreloaded Statement
hi def link newlispObsolete Todo
hi def link newlispDebugging Todo
hi def link newlispColon Type
hi def link newlispComma Type
hi def link newlispBoolean Boolean
hi def link newlispError Error

" CommentItalic and CommentUnderlined groups have the same colors as
" plain Comment, but italic and underlined respectively. Therefore we
" need to recalculate those attributes on each colorscheme change.
"
" But we can't implement this for old Vim versions and semi-compatible
" highlighting tools, so defaults are supplied as a fallback.

hi link CommentItalic Identifier
hi link CommentUnderlined Underlined

function! s:color_of(where, what)
  let val = synIDattr(hlID("Comment"), a:what, a:where)
  return val == "" || val == -1 ? "" : printf(" %s%s=%s", a:where, a:what, val)
endfunction

function! s:set_colors()
  let colors =  s:color_of("cterm", "fg") . s:color_of("cterm", "bg") . s:color_of("gui", "fg") . s:color_of("gui", "bg")
  exec "hi CommentItalic term=italic cterm=italic gui=italic" . colors
  exec "hi CommentUnderlined term=underline cterm=underline gui=underline" . colors
endfunction

if version >= 700 && !exists("g:newlisp_compat")
  au ColorScheme <buffer> call s:set_colors()
  call s:set_colors()
endif

let b:current_syntax = "newlisp"

" vim: textwidth=70
