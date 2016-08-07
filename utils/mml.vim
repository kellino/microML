"
" this syntax file is based on that for SML 
" maintained by Markus Mottl            <markus.mottl@gmail.com>
"               Fabrizio Zeno Cornelli  <zeno@filibusta.crema.unimi.it>
"
"
" Vim syntax file
" Language:     microML
" Filenames:    *.mml 
" Maintainers:  David Kelly   <dkellino@gmail.com>
" URL:          http://www.github.com/kellino/microML
" Last Change:  07 August 2016

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if v:version < 600
  syntax clear
elseif exists('b:current_syntax')
  finish
endif

" microML is case sensitive.
syn case match

" lowercase identifier - the standard way to match
syn match    mmlLCIdentifier /\<\(\l\|_\)\(\w\|'\)*\>/

syn match    mmlKeyChar    "|"

" Errors
syn match    mmlBrackErr   "\]"
syn match    mmlParenErr   ")"
syn match    mmlCommentErr "\*)"
syn match    mmlThenErr    "\<then\>"

" Some convenient clusters
syn cluster  mmlAllErrs contains=mmlBrackErr,mmlParenErr,mmlCommentErr,mmlThenErr

syn cluster  mmlAENoParen contains=mmlBraceErr,mmlBrackErr,mmlCommentErr,mmlEndErr,mmlThenErr

syn cluster  mmlContained contains=mmlTodo,mmlPreDef,mmlModParam,mmlModParam1,mmlPreMPRestr,mmlMPRestr,mmlMPRestr1,mmlMPRestr2,mmlMPRestr3,mmlModRHS,mmlFuncWith,mmlFuncStruct,mmlModTypeRestr,mmlModTRWith,mmlWith,mmlWithRest,mmlModType,mmlFullMod,mmlRecordField


" Enclosing delimiters
syn region   mmlEncl transparent matchgroup=mmlKeyword start="(" matchgroup=mmlKeyword end=")" contains=ALLBUT,@mmlContained,mmlParenErr
syn region   mmlEncl transparent matchgroup=mmlKeyword start="\[" matchgroup=mmlKeyword end="\]" contains=ALLBUT,@mmlContained,mmlBrackErr
syn region   mmlEncl transparent matchgroup=mmlKeyword start="#\[" matchgroup=mmlKeyword end="\]" contains=ALLBUT,@mmlContained,mmlBrackErr


" Comments
syn region   mmlComment start="(\*" end="\*)" contains=mmlComment,mmlTodo
syn region   mmlLineComment start="--" end="\n" contains=mmlString, mmlTodo
syn keyword  mmlTodo contained TODO FIXME XXX

" let
syn region   mmlEnd matchgroup=mmlKeyword start="\<let\>" matchgroup=mmlKeyword end="\<end\>" contains=ALLBUT,@mmlContained,mmlEndErr

" local
syn region   mmlEnd matchgroup=mmlKeyword start="\<local\>" matchgroup=mmlKeyword end="\<end\>" contains=ALLBUT,@mmlContained,mmlEndErr

" abstype
syn region   mmlNone matchgroup=mmlKeyword start="\<abstype\>" matchgroup=mmlKeyword end="\<end\>" contains=ALLBUT,@mmlContained,mmlEndErr

" begin
syn region   mmlEnd matchgroup=mmlKeyword start="\<begin\>" matchgroup=mmlKeyword end="\<end\>" contains=ALLBUT,@mmlContained,mmlEndErr

" if
syn region   mmlNone matchgroup=mmlKeyword start="\<if\>" matchgroup=mmlKeyword end="\<then\>" contains=ALLBUT,@mmlContained,mmlThenErr

" "open"
syn region   mmlNone matchgroup=mmlKeyword start="\<open\>" matchgroup=mmlModule end="\<\u\(\w\|'\)*\(\.\u\(\w\|'\)*\)*\>" contains=@mmlAllErrs,mmlComment

" "structure" - somewhat complicated stuff ;-)
syn region   mmlModule matchgroup=mmlKeyword start="\<\(structure\|functor\)\>" matchgroup=mmlModule end="\<\u\(\w\|'\)*\>" contains=@mmlAllErrs,mmlComment skipwhite skipempty nextgroup=mmlPreDef
syn region   mmlPreDef start="."me=e-1 matchgroup=mmlKeyword end="\l\|="me=e-1 contained contains=@mmlAllErrs,mmlComment,mmlModParam,mmlModTypeRestr,mmlModTRWith nextgroup=mmlModPreRHS
syn region   mmlModParam start="([^*]" end=")" contained contains=@mmlAENoParen,mmlModParam1
syn match    mmlModParam1 "\<\u\(\w\|'\)*\>" contained skipwhite skipempty nextgroup=mmlPreMPRestr

syn region   mmlPreMPRestr start="."me=e-1 end=")"me=e-1 contained contains=@mmlAllErrs,mmlComment,mmlMPRestr,mmlModTypeRestr

syn region   mmlMPRestr start=":" end="."me=e-1 contained contains=@mmlComment skipwhite skipempty nextgroup=mmlMPRestr1,mmlMPRestr2,mmlMPRestr3
syn region   mmlMPRestr1 matchgroup=mmlModule start="\ssig\s\=" matchgroup=mmlModule end="\<end\>" contained contains=ALLBUT,@mmlContained,mmlEndErr,mmlModule
syn region   mmlMPRestr2 start="\sfunctor\(\s\|(\)\="me=e-1 matchgroup=mmlKeyword end="->" contained contains=@mmlAllErrs,mmlComment,mmlModParam skipwhite skipempty nextgroup=mmlFuncWith
syn match    mmlMPRestr3 "\w\(\w\|'\)*\(\.\w\(\w\|'\)*\)*" contained
syn match    mmlModPreRHS "=" contained skipwhite skipempty nextgroup=mmlModParam,mmlFullMod
syn region   mmlModRHS start="." end=".\w\|([^*]"me=e-2 contained contains=mmlComment skipwhite skipempty nextgroup=mmlModParam,mmlFullMod
syn match    mmlFullMod "\<\u\(\w\|'\)*\(\.\u\(\w\|'\)*\)*" contained skipwhite skipempty nextgroup=mmlFuncWith

syn region   mmlFuncWith start="([^*]"me=e-1 end=")" contained contains=mmlComment,mmlWith,mmlFuncStruct
syn region   mmlFuncStruct matchgroup=mmlModule start="[^a-zA-Z]struct\>"hs=s+1 matchgroup=mmlModule end="\<end\>" contains=ALLBUT,@mmlContained,mmlEndErr

syn match    mmlModTypeRestr "\<\w\(\w\|'\)*\(\.\w\(\w\|'\)*\)*\>" contained
syn region   mmlModTRWith start=":\s*("hs=s+1 end=")" contained contains=@mmlAENoParen,mmlWith
syn match    mmlWith "\<\(\u\(\w\|'\)*\.\)*\w\(\w\|'\)*\>" contained skipwhite skipempty nextgroup=mmlWithRest
syn region   mmlWithRest start="[^)]" end=")"me=e-1 contained contains=ALLBUT,@mmlContained

" "signature"
syn region   mmlKeyword start="\<signature\>" matchgroup=mmlModule end="\<\w\(\w\|'\)*\>" contains=mmlComment skipwhite skipempty nextgroup=mmlMTDef
syn match    mmlMTDef "=\s*\w\(\w\|'\)*\>"hs=s+1,me=s

syn keyword  mmlKeyword  if then else
syn keyword  mmlKeyword  let in case of

syn keyword  mmlType     Boolean Number Char String

syn keyword  mmlOperator and or not xor

syn keyword  mmlBoolean      true false
syn match    mmlConstructor  "(\s*)"
syn match    mmlConstructor  "\[\s*\]"
syn match    mmlConstructor  "#\[\s*\]"
syn match    mmlConstructor  "\u\(\w\|'\)*\>"

syn match    mmlCharacter    +#"\\""\|#"."\|#"\\\d\d\d"+
syn match    mmlCharErr      +#"\\\d\d"\|#"\\\d"+
syn region   mmlString       start=+"+ skip=+\\\\\|\\"+ end=+"+

syn match    mmlKeyChar      "="
syn match    mmlKeyChar      ";"
syn match    mmlKeyChar      "\*"
syn match    mmlFunDef       "=>"
syn match    mmlRefAssign    ":="
syn match    mmlOperator     "\^"
syn match    mmlOperator     "::"
syn match    mmlAnyVar       "\<_\>"

syn match    mmlNumber	      "\<-\=\d\+\>"
syn match    mmlNumber	      "\<-\=0[x|X]\x\+\>"
syn match    mmlReal	      "\<-\=\d\+\.\d*\([eE][-+]\=\d\+\)\=[fl]\=\>"

syn match    mmlRecordField   "\<\w\+\>\(\s*[=:]\)\@=" contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if v:version >= 508 || !exists('did_mml_syntax_inits')
  if v:version < 508
    let g:did_mml_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink mmlBraceErr	 Error
  HiLink mmlBrackErr	 Error
  HiLink mmlParenErr	 Error

  HiLink mmlCommentErr	 Error

  HiLink mmlEndErr	 Error
  HiLink mmlThenErr	 Error

  HiLink mmlCharErr	 Error

  HiLink mmlComment	 Comment
  HiLink mmlLineComment  Comment

  HiLink mmlModPath	 Include
  HiLink mmlModule	 Include
  HiLink mmlModParam1	 Include
  HiLink mmlModType	 Include
  HiLink mmlMPRestr3	 Include
  HiLink mmlFullMod	 Include
  HiLink mmlModTypeRestr Include
  HiLink mmlWith	 Include
  HiLink mmlMTDef	 Include

  HiLink mmlConstructor  Constant

  HiLink mmlModPreRHS	 Keyword
  HiLink mmlMPRestr2	 Keyword
  HiLink mmlKeyword	 Keyword
  HiLink mmlFunDef	 Keyword
  HiLink mmlRefAssign	 Keyword
  HiLink mmlKeyChar	 Keyword
  HiLink mmlAnyVar	 Keyword
  HiLink mmlTopStop	 Keyword
  HiLink mmlOperator	 Keyword

  HiLink mmlRecordField  Identifier

  HiLink mmlBoolean	 Boolean
  HiLink mmlCharacter	 Character
  HiLink mmlNumber	 Number
  HiLink mmlReal	 Float
  HiLink mmlString	 String
  HiLink mmlType	 Type
  HiLink mmlTodo	 Todo
  HiLink mmlEncl	 Keyword

  delcommand HiLink
endif

let b:current_syntax = 'mml'

" vim: ts=8
