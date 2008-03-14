" Vim syntax file
" Language:     newLISP
" Maintainer:   Cyril Slobin <slobin@ice.ru>
" URL:          http://www.vim.org/scripts/script.php?script_id=2067
" Another URL:  http://wagner.pp.ru/~slobin/vim/syntax/newlisp.vim
" Started at:   2007 Nov 07 (The Great Revolution 90th Anniversary)
" Last change:  2008 Feb 28
" newLISP site: http://www.newlisp.org/

" $Id: newlisp.vim,v 1.19 2008-02-28 21:10:00+03 slobin Exp $

" This was the alternative Vim syntax file for the newLISP language.
" Now it is the official Vim syntax file! I am a celebrity! Wow!

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
" * Starting from version 1.13 of this file, the feature of selective
"   errors disabling was itself disabled. If you want NOT to highlight
"   nested left parenthesis in the first column, just put the line:
"
"     hi link newlispLeftParenError newlispParenthesis
"
"   in your .vimrc file, similarly for all other errors.
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
syn match newlispSymbolSpecial "\<[A-Z_][0-9A-Z_]*\>"

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

" This keywords list is based on newLISP v.9.3 release primes.h file 

syn keyword newlispFunction ! != $ % & * + - / : < << <= = > >= >> NaN? ^ abs acos acosh add address
syn keyword newlispFunction amb and append append-file apply args array array-list array? asin asinh
syn keyword newlispFunction assoc assoc-set atan atan2 atanh atom? base64-dec base64-enc bayes-query
syn keyword newlispFunction bayes-train begin beta betai bind binomial callback case catch ceil
syn keyword newlispFunction change-dir char chop clean close command-line cond cons constant context
syn keyword newlispFunction context? copy-file cos cosh count cpymem crc32 crit-chi2 crit-z
syn keyword newlispFunction current-line curry date date-value debug dec def-new default define
syn keyword newlispFunction define-macro delete delete-file delete-url destroy det device difference
syn keyword newlispFunction directory directory? div do-until do-while doargs dolist dostring
syn keyword newlispFunction dotimes dotree dump dump-symbol dup empty? encrypt ends-with env erf
syn keyword newlispFunction error-event error-number error-text eval eval-string exec exists exit
syn keyword newlispFunction exp expand explode factor fft file-info file? filter find find-all first
syn keyword newlispFunction flat float float? floor flt for for-all fork format fv gammai gammaln
syn keyword newlispFunction gcd get-char get-float get-int get-long get-string get-url global
syn keyword newlispFunction global? if ifft import inc index int integer integer? intersect invert
syn keyword newlispFunction irr join lambda? last legal? length let letex letn list list? load local
syn keyword newlispFunction log lookup lower-case macro? main-args make-dir map mat match max member
syn keyword newlispFunction min mod mul multiply name net-accept net-close net-connect net-error
syn keyword newlispFunction net-eval net-listen net-local net-lookup net-peek net-peer net-ping
syn keyword newlispFunction net-receive net-receive-from net-receive-udp net-select net-send
syn keyword newlispFunction net-send-to net-send-udp net-service net-sessions new nil? normal not
syn keyword newlispFunction now nper npv nth nth-set null? number? open or pack parse parse-date
syn keyword newlispFunction peek pipe pmt pop pop-assoc post-url pow pretty-print primitive? print
syn keyword newlispFunction println prob-chi2 prob-z process protected? push put-url pv quote quote?
syn keyword newlispFunction rand random randomize read-buffer read-char read-file read-key read-line
syn keyword newlispFunction real-path ref ref-all ref-set regex remove-dir rename-file replace
syn keyword newlispFunction replace-assoc reset rest reverse rotate round save search seed seek
syn keyword newlispFunction select semaphore sequence series set set-assoc set-locale set-nth
syn keyword newlispFunction set-ref set-ref-all setq sgn share signal silent sin sinh sleep slice
syn keyword newlispFunction sort source sqrt starts-with string string? sub swap sym symbol? symbols
syn keyword newlispFunction sys-error sys-info tan tanh throw throw-error time time-of-day timer
syn keyword newlispFunction title-case trace trace-highlight transpose trim true? unicode unify
syn keyword newlispFunction unique unless unpack until upper-case utf8 utf8len uuid wait-pid when
syn keyword newlispFunction while write-buffer write-char write-file write-line xml-error xml-parse
syn keyword newlispFunction xml-type-tags zero? \| ~

syn keyword newlispFunction read " introduced in newLISP 9.3.1

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

au ColorScheme <buffer> call s:set_colors()
call s:set_colors()

let b:current_syntax = "newlisp"

" vim: textwidth=70
