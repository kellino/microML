" shamelessly ripped off from Twinside/vim-haskellFold on github

setlocal foldmethod=expr
setlocal foldexpr=GetMMLFold(v:lnum)

"function! GetMMLFold(lnum)
    "if getline(a:lnum) =~? '\v^\s*$'
        "return '-1'
    "endif
    "return '0'
"endfunction

function! GetMMLFold(lnum) 
    let l:line = getline( a:lnum )

    " Beginning of comment
    if l:line =~? '\v^\s*--' || l:line =~? '\v^\s*(\*'
        return '2'
    endif

    if l:line =~? '\v^\s*$'
        let l:nextline = getline(a:lnum + 1)
        if l:nextline =~# '^--' || l:nextline =~? '^(\*'
            return '0'
        else
            return '-1'
        endif
    endif
    return '1'
endfunction 


"fun! MMLFoldText() "{{{
    "let l:i = v:foldstart
    "let l:retVal = ''
    "let l:began = 0

    "let l:commentOneLine = '^\s*--.*$'
    "let l:monoLineComment = '^\s*--.*$'
    "let l:emptyLine = '^\s*$'
    "let l:nonEmptyLine = '^\s\+\S'
    "let l:multiLineCommentBegin = '^\s*(\*'
    "let l:multiLineCommentEnd = '\*)'

    "let l:isMultiLine = 0

    "let l:line = getline(l:i)

    "while l:i <= v:foldend
        "if l:isMultiLine 
            "if l:line =~ l:multiLineCommentEnd
                "let l:isMultiLine = 0
                "let l:line = substitute(l:line, '.*\*)', '', '')

                "if l:line =~ l:emptyLine
                    "let l:i = l:i + 1
                    "let l:line = getline(l:i)
                "end
            "else
                "let l:i = l:i + 1
                "let l:line = getline(l:i)
            "end
        "else
            "if l:line =~ l:multiLineCommentBegin
                "let l:isMultiLine = 1
                "continue
            "elseif l:began == 0 && !(l:line =~ l:commentOneLine)
                "let l:retVal = substitute(l:line, l:monoLineComment, ' ', '')
                "let l:began = 1
            "elseif l:began != 0 && l:line =~ l:nonEmptyLine 
                "let l:tempVal = substitute(l:line, '\s\+\(.*\)$', ' \1', '')
                "let l:retVal = l:retVal . substitute(l:tempVal, '\s\+--.*', ' ', '')
            "elseif l:began != 0
                "break
            "endif

            "let l:i = l:i + 1
            "let l:line = getline(l:i)
        "endif
    "endwhile

    "if l:retVal ==# ''
        "return foldtext()
    "endif
    
    "return l:retVal
"endfunction "}}}


"fun! s:setMMLFolding() "{{{
    "setlocal foldexpr=MMLFold(v:lnum)
    "setlocal foldtext=MMLFoldText()
    "setlocal foldmethod=expr
"endfunction "}}}

"augroup MMLFold
    "au!
    "au FileType mml call s:setMMLFolding()
"augroup END
