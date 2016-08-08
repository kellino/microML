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
"syn region   mmlEnd matchgroup=mmlKeyword start="\<let\>" contains=ALLBUT,@mmlContained,mmlEndErr
syn match    mmlLet "\<let\>"

" if
syn region   mmlNone matchgroup=mmlKeyword start="\<if\>" matchgroup=mmlKeyword end="\<then\>" contains=ALLBUT,@mmlContained,mmlThenErr

syn keyword  mmlKeyword  if then else and or not xor
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
syn match    mmlKeyChar      "<"
syn match    mmlKeyChar      ">"
syn match    mmlKeyChar      "%"
syn match    mmlKeyChar      ";"
syn match    mmlKeyChar      "\*"
syn match    mmlFunDef       "->"
syn match    mmlOperator     "\^"
syn match    mmlOperator     "/"
syn match    mmlOperator     ":"
syn match    mmlAnyVar       "\<_\>"

syn match    mmlNumber	      "\<-\=\d\+\>"
syn match    mmlNumber	      "\<-\=0[x|X]\x\+\>"

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

  HiLink mmlKeyword      Type
  HiLink mmlLet          Type

  HiLink mmlCommentErr	 Error

  HiLink mmlEndErr	 Error
  HiLink mmlThenErr	 Error

  HiLink mmlCharErr	 Error

  HiLink mmlComment	 Comment
  HiLink mmlLineComment  Comment

  HiLink mmlKeyChar	 Keyword
  HiLink mmlAnyVar	 Keyword
  HiLink mmlTopStop	 Keyword
  HiLink mmlOperator	 Keyword

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
