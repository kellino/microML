runtime autoload/funcs.vim

function! microMLcomplete#CompleteMicroML(findstart, base) abort
  if a:findstart
    " locate the start of the word
    let l:line = getline('.')
    let l:start = col('.') - 1
    while l:start > 0 && l:line[l:start - 1] =~ '\a'
      let l:start -= 1
    endwhile
    return l:start
  else
    let l:res = []
    for l:m in g:funcs
      if l:m =~ '^' . a:base
        call add(l:res, l:m)
      endif
    endfor
    return l:res
  endif
endfun
