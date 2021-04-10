au BufEnter no_plugins.vim command! Pline execute 'echo getline(".")'
au BufEnter no_plugins.vim command! START normal gg8Gzz:Pline<CR>
au BufEnter no_plugins.vim command! GO normal M17jzzH:Pline<CR>
au BufEnter no_plugins.vim command! BACK normal M17kzzH:Pline<CR>
au BufEnter no_plugins.vim nnoremap <buffer> . :START<CR>
au BufEnter no_plugins.vim nnoremap <buffer> <Down> :GO<CR>
au BufEnter no_plugins.vim nnoremap <buffer> <Up> :BACK<CR>
au BufEnter no_plugins.vim setl window=66 | exe 'START' | call HideAll()
au BufLeave no_plugins.vim call ShowAll()

" toggle hiding everything except for the presentation contents
let s:hidden_all=0
function! ToggleHiddenAll()
    echom 's:hidden_all:' . s:hidden_all
    if s:hidden_all == 0
        call HideAll()
    else
        call ShowAll()
    endif
endfunction
function! HideAll()
    let s:hidden_all=1
    setl noruler
    setl laststatus=0
    setl nonumber
    setl norelativenumber
    setl listchars=tab:>\ ,trail:-,nbsp:+
    " setl noshowmode
    " setl noshowcmd
    " setl shortmess=F
endfunction
function! ShowAll()
    let s:hidden_all=0
    setl ruler
    setl laststatus=2
    setl number
    setl relativenumber
    set listchars=tab:»\ ,trail:▫,eol:¬,extends:>,precedes:<,nbsp:␣,conceal:┊
    " setl showmode
    " setl showcmd
    " setl shortmess=filnxtToOF
endfunction
nnoremap <Leader><Leader>h :call ToggleHiddenAll()<CR>




" 如果需要解绑键位 如下:
" au BufEnter no_plugins.vim unmap H
" au BufEnter no_plugins.vim unmap L
