" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <slobin@ice.ru>
" URL:          http://www.vim.org/scripts/script.php?script_id=2067
" Another URL:  http://wagner.pp.ru/~slobin/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th Anniversary)
" Last change:  2008 Jun 19
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.22 2008/06/19 23:00:00 slobin Exp $

" This version of the syntax file attempts to meet the newLISP v.9.4.0

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
"   Where have you dug them out? In fact I haven't tested it even with
"   version 6.x - I all do my development with 7.1.175.

if exists("b:current_syntax")
  finish
endif

syntax case match

setlocal iskeyword=33,36-38,42,43,45-47,48-57,60-64,@,92,94,_,124,126

syn region newlispComment oneline start="[;#]" end="$" contains=newlispTodo,@Spell
syn keyword newlispTodo FIXME TODO XXX contained

syn region newlispDocComment start="^;;\(\s\|$\)" end="^\(;;\(\s\|$\)\)\@!" contains=newlispTodo,newlispDocExample,newlispDocKeyword,newlispDocItalic,newlispDocMonospace,newlispDocHTMLTag,newlispDocHTMLEntity,@Spell
syn region newlispDocExample matchgroup=newlispDocKeyword start="^;;\s@example$"ms=s+3 matchgroup=NONE end="^\(;;\(\s\|$\)\)\@!" contained
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

" This keywords list is based on newLISP v.9.3.15 build primes.h file
" XXX Don't forget to remove colon ":" and escape vertical bar "|"

syn keyword newlispFunction ! != $ % & * + - / < << <= = > >= >> NaN? ^ abort abs acos acosh add
syn keyword newlispFunction address amb and append append-file apply args array array-list array?
syn keyword newlispFunction asin asinh assoc assoc-set atan atan2 atanh atom? base64-dec base64-enc
syn keyword newlispFunction bayes-query bayes-train begin beta betai bind binomial callback case
syn keyword newlispFunction catch ceil change-dir char chop clean close command-event cond cons
syn keyword newlispFunction constant context context? copy-file cos cosh count cpymem crc32
syn keyword newlispFunction crit-chi2 crit-z current-line curry date date-value debug dec def-new
syn keyword newlispFunction default define define-macro delete delete-file delete-url destroy det
syn keyword newlispFunction device difference directory directory? div do-until do-while doargs
syn keyword newlispFunction dolist dostring dotimes dotree dump dump-symbol dup empty? encrypt
syn keyword newlispFunction ends-with env erf error-event error-number error-text eval eval-string
syn keyword newlispFunction exec exists exit exp expand explode factor fft file-info file? filter
syn keyword newlispFunction find find-all first flat float float? floor flt for for-all fork format
syn keyword newlispFunction fv gammai gammaln gcd get-char get-float get-int get-long get-string
syn keyword newlispFunction get-url global global? if ifft import inc index int integer integer?
syn keyword newlispFunction intersect invert irr join lambda? last legal? length let letex letn list
syn keyword newlispFunction list? load local log lookup lower-case macro? main-args make-dir map mat
syn keyword newlispFunction match max member min mod mul multiply name net-accept net-close
syn keyword newlispFunction net-connect net-error net-eval net-listen net-local net-lookup net-peek
syn keyword newlispFunction net-peer net-ping net-receive net-receive-from net-receive-udp
syn keyword newlispFunction net-select net-send net-send-to net-send-udp net-service net-sessions
syn keyword newlispFunction new nil? normal not now nper npv nth nth-set null? number? open or pack
syn keyword newlispFunction parse parse-date peek pipe pmt pop pop-assoc post-url pow pretty-print
syn keyword newlispFunction primitive? print println prob-chi2 prob-z process prompt-event
syn keyword newlispFunction protected? push put-url pv quote quote? rand random randomize
syn keyword newlispFunction read-buffer read-char read-expr read-file read-key read-line real-path
syn keyword newlispFunction ref ref-all ref-set regex regex-comp remove-dir rename-file replace
syn keyword newlispFunction replace-assoc reset rest reverse rotate round save search seed seek
syn keyword newlispFunction select semaphore sequence series set set-assoc set-locale set-nth
syn keyword newlispFunction set-ref set-ref-all setq sgn share signal silent sin sinh sleep slice
syn keyword newlispFunction sort source spawn sqrt starts-with string string? sub swap sym symbol?
syn keyword newlispFunction symbols sync sys-error sys-info tan tanh throw throw-error time
syn keyword newlispFunction time-of-day timer title-case trace trace-highlight transpose trim true?
syn keyword newlispFunction unicode unify unique unless unpack until upper-case utf8 utf8len uuid
syn keyword newlispFunction wait-pid when while write-buffer write-char write-file write-line
syn keyword newlispFunction xml-error xml-parse xml-type-tags zero? \| ~

syn keyword newlispKeyword fn lambda
syn keyword newlispVariable ostype $0 $1 $2 $3 $4 $5 $6 $7 $8 $9 $10 $11 $12 $13 $14 $15 $args $idx $main-args

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
hi def link newlispKeyword Statement
hi def link newlispVariable Statement
hi def link newlispColon Type
hi def link newlispComma Type
hi def link newlispBoolean Boolean
hi def link newlispError Error

" CommentItalic and CommentUnderlined groups have the same colors as
" plain Comment, but italic and underlined respectively. Therefore we
" need to recalculate those attributes on each colorscheme change.

function! s:color_of(where, what)
  let val = synIDattr(hlID("Comment"), a:what, a:where)
  return val == "" || val == -1 ? "" : printf(" %s%s=%s", a:where, a:what, val)
endfunction

function! s:set_colors()
  let colors =  s:color_of("cterm", "fg") . s:color_of("cterm", "bg") . s:color_of("gui", "fg") . s:color_of("gui", "bg")
  exec "hi CommentItalic term=italic cterm=italic gui=italic" . colors
  exec "hi CommentUnderlined term=underline cterm=underline gui=underline" . colors
endfunction

if version < 700 || exists("g:newlisp_compat")
  hi def link CommentItalic Identifier
  hi def link CommentUnderlined Underlined
else
  au ColorScheme <buffer> call s:set_colors()
  call s:set_colors()
endif

let b:current_syntax = "newlisp"

" vim: textwidth=70

