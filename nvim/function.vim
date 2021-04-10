" Compile Function
    nnoremap <M-r> :call CompileRunGcc()<CR>
    func! CompileRunGcc()
        exec "w"
        if &filetype == 'c'
            " exec "!g++ % -o %<"
            " exec "!gcc % -o %<"
            " exec "!time ./%<"
            :FloatermNew gcc % -o %< && time ./%<
        elseif &filetype == 'cpp'
            " set splitbelow
            " exec "!g++ -std=c++11 % -Wall -o %<"
            " :sp
            " :res -15
            " :term ./%<
            :FloatermNew g++ -std=c++11 % -Wall -o %< && ./%<
        elseif &filetype == 'java'
            " ==== compile & run ===
            " exec "!javac % && time java %<"
            " :FloatermNew --width=80 --height=40 javac % && time java %<
            :FloatermNew javac % && time java %<
            " === make & run ===
            " exec 'set makeprg=javac\ -g\ %'
            " exec "make"
            " exec "!time java %<"
            " === make ===
            " exec "make"
            " === for debug ===
            " exec "!time java -Xdebug -Xrunjdwp:server=y,transport=dt_socket,address=5005,suspend=y %<"
        elseif &filetype == 'rust'
            :FloatermNew rustc % && time ./%<
        elseif &filetype == 'sh'
            " :!time bash %
            :FloatermNew time bash %
        elseif &filetype == 'python'
            " set splitbelow
            " :sp
            " :term python3 %
            :FloatermNew python3 %
        elseif &filetype == 'html'
            silent! exec "!".g:mkdp_browser." % &"
        elseif &filetype == 'vimwiki'
            exec "MarkdownPreview"
        elseif &filetype == 'markdown'
            exec "MarkdownPreview"
        elseif &filetype == 'tex'
            " silent! exec "VimtexStop"
            " silent! exec "VimtexCompile"
            silent! exec "LLPStartPreview"
        elseif &filetype == 'dart'
            CocCommand flutter.run -d iPhone\ 11\ Pro
            CocCommand flutter.dev.openDevLog
        elseif &filetype == 'javascript'
            " set splitbelow
            " :sp
            " :term export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings .
            :FloatermNew export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings .
        elseif &filetype == 'go'
            " set splitbelow
            " :sp
            " :term go run .
            :FloatermNew go run %
        elseif &filetype == 'nasm'
            exec "!nasm -f bin % -o %<.bin"
        elseif &filetype == 'lua'
            :FloatermNew time lua %
        endif
    endfunc

" Quickfix Toggle
    " Copen comes with vim-dispatch, copen is builtin
    " nnoremap <Leader>qf :Copen<CR>
    nnoremap <Leader>qf :QFix<CR>
    " let t:qfix_win=2
    let g:jah_Quickfix_Win_Height=10
    " toggles the quickfix window.
    command! -bang -nargs=? QFix call QFixToggle(<bang>0)
    function! QFixToggle(forced)
    if exists("g:qfix_win") && a:forced == 0
        cclose
    else
        execute "copen " . g:jah_Quickfix_Win_Height
    endif
    endfunction
    " used to track the quickfix window
    augroup QFixToggle
    autocmd!
    autocmd BufWinEnter quickfix let g:qfix_win = bufnr("$")
    autocmd BufWinLeave * if exists("g:qfix_win") && expand("<abuf>") == g:qfix_win | unlet! g:qfix_win | endif
    augroup END

" Another Quickfix Toggle --testing
    " another quickfix toggle --testing
    function! GetBufferList()
    redir =>buflist
    silent! ls!
    redir END
    return buflist
    endfunction

    function! ToggleList(bufname, pfx)
    let buflist = GetBufferList()
    for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "'.a:bufname.'"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
        if bufwinnr(bufnum) != -1
        exec(a:pfx.'close')
        return
        endif
    endfor
    if a:pfx == 'l' && len(getloclist(0)) == 0
        echohl ErrorMsg
        echo "Location List is Empty."
        return
    endif
    let winnr = winnr()
    exec(a:pfx.'open')
    if winnr() != winnr
        wincmd p
    endif
    endfunction

    " nmap <silent> <Leader>`l :call ToggleList("Location List", 'l')<CR>
    " nmap <silent> <Leader>`q :call ToggleList("Quickfix List", 'c')<CR>

" Sessions
    " 自带的session颜色丢失 用startify的session管理代替
    " auto save and load session for $HOME/.config/nvim/session/current.session
    " set sessionoptions="blank,buffers,curdir,folds,globals,help,localoptions,options,resize,sesdir,slash,tabpages,terminal,unix,winpos,winsize"
    " set sessionoptions="blank,buffers,folds,globals,help,localoptions,resize,sesdir,slash,tabpages,terminal,unix,winpos,winsize"
    " let g:OrigPWD=getcwd()
    " let g:OrigPWD='/home/xc/.config/nvim'
    " let g:AutoSessionFile='session/recent.session'
    " if filereadable("".g:OrigPWD."/".g:AutoSessionFile)
    "     if argc() == 0
    "         au VimEnter * call LoadRecentSession()
    "     endif
    " else
    "     if argc() == 0
    "         au VimLeave * call SaveRecentSession()
    "     endif
    " endif
    " function! SaveRecentSession()
    "     exec "mks! ".g:OrigPWD."/".g:AutoSessionFile
    " endfunction
    " function! LoadRecentSession()
    "     exe "source ".g:OrigPWD."/".g:AutoSessionFile
    " endfunction

" Format And Encoding
    command! -bang -nargs=* UnixEncodingUtf8 exec "set fileformat=unix | set fileencoding=utf-8"
    " <q-args>会自动对参数特殊字符进行转义 函数接收参数时，使用关键字<f-args>
    com! -bang -nargs=* FormatAndEncode call FormatAndEncodeFunc(<f-args>)
    " 可变参数（Varargs)：Vimscript 函数支持可变参数传递，参数格式为：...
    " func! FormatAndEncodeFunc(...)
    " usage: a:0 a:1 a:2
    func! FormatAndEncodeFunc(format, encoding)
        execute 'set fileformat=' . a:format
        execute 'set fileencoding=' . a:encoding
    endfunc

" Netrw Settings
    nnoremap <Leader>tt :Lexplore<CR><C-w>l
    " @see https://vi.stackexchange.com/questions/22455/how-to-override-netrw-delete-behavior
    " unfortunately, the visual mode is not supported by Netrw_UserMaps,
    " so implement it with the help of augroup/autocmd
    " (although netrw is very peculiar about its mappings,
    " it seems to work okay too)
    augroup MyNetrw | au!
        autocmd FileType netrw nnoremap <buffer>r :call g:MyNetrw_nop(1)<CR>
    augroup end
    " setup netrw mappings (:h g:Netrw_UserMaps)
    let g:Netrw_UserMaps = [['<C-l>', 'g:MyNetrw_ctrl_l'], ['s', 'g:MyNetrw_nop']]
    " implement normal mode refresh; islocal:1 stands for true
    function! g:MyNetrw_nop(islocal)
        echom 'not supported operation'
    endfunction
    function! g:MyNetrw_ctrl_l(islocal)
        if a:islocal
            return 'refresh'
        endif
        " this is to handle remote deletion
        echom 'not supported operation'
    endfunction
    let g:netrw_banner=1       " set 0 if you want to disable annoying banner
    let g:netrw_browse_split=4 " open in prior window
    let g:netrw_altv=1         " open splits to the right
    let g:netrw_liststyle=3    " tree view
    let g:netrw_list_hide=netrw_gitignore#Hide()
    " let g:netrw_list_hide.=',\(^\|\s\s\)\zs\.\S\+'
    let g:netrw_winsize=15
    let g:netrw_list_hide='
                \.*.netrwhist$,
                \.*.swp$,
                \ *.pyc$,
                \ *.log$,
                \ *.o$,
                \ *.xmi$,
                \ *.swp$,
                \ *.bak$,
                \ *.class$,
                \ *.pyc$,
                \ *.jar$,
                \ *.war$,
                \ *__pycache__*
                \'

" CTRL-W And Meta-D
    " 检查是否删除到了行首
    function! s:is_first_of_line() abort
        return !(col('.') - 1)
    endfunction
    " 检查是否删除到了行尾 注意vim行尾有换行符, 所以 col(最后一个可视字符) + 1 = col('$')
    function! s:is_end_of_line() abort
        return col('.') == col('$')
    endfunction
    " <C-w> 向行首删除一个word; note:只在当前行删除
    inoremap <silent><expr> <C-w>
        \ <SID>is_first_of_line() ? "" :
        \ "<C-w>"
    " <M-d> 向行尾删除一个word; note:只在当前行删除
    inoremap <silent><expr> <M-d>
        \ <SID>is_end_of_line() ? "" :
        \ "<C-o>de"

" RestoreRegister
    " prevent replacing paste buffer on paste
    function! RestoreRegister()
    let @" = s:restore_reg
    return ''
    endfunction
    function! s:Repl()
    let s:restore_reg = @"
    return "p@=RestoreRegister()\<CR>"
    endfunction
    vmap <silent> <expr> p <sid>Repl()

" SynStack
    " describe syntax under cursor
    function! <SID>SynStack()
        if !exists("*synstack")
            return
        endif
        echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
    endfunc
    nnoremap <Leader>ds :call <SID>SynStack()<CR>
