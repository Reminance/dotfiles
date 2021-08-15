let g:dbadmin_path='/home/xc/workspace/work-tools/dbadmin'
let g:dbadmin_db='wms'
let g:dbadmin_page=1
let g:dbadmin_limit=100
let g:dbadmin_output='csv'
" let g:dbadmin_output='json'
let g:dbadmin_sql_no_cache='true'

let g:logcenter_path='/home/xc/workspace/work-tools/logcenter'

xnoremap <leader><leader>d :<C-U> call ExecuteSql('false')<Cr>
xnoremap <leader><leader>D :<C-U> call ExecuteSql('true')<Cr>
" xnoremap <leader><leader>l y:tabe<CR>:tabmove<CR>:term <C-r>"<CR>:setl nonu<CR>:setl nornu<CR>
xnoremap <leader><leader>l y:tabe<CR>:term <C-r>"<CR>:setl nonu<CR>:setl nornu<CR>A
nnoremap <leader><leader>s :SwitchDB 

" ################################ SwitchDB ################################
command! -complete=shellcmd -nargs=+ SwitchDB call SwitchDB(<q-args>)
function! SwitchDB(db)
    let g:dbadmin_db=a:db
endfunction

" ################################ ExecuteSql ################################
function! ExecuteSql(explain)
    let sql=GetVisualSelection(visualmode())
    " for multiline shell commnd
    let sql=substitute(sql, "\"", "'", "g")
    let sql=substitute(sql, "\\n", " ", "g")
    let sql=substitute(sql, ' \+', " ", "g")
    if StartsWith(sql, "./dbadmin")
        let shellCmdStr=substitute(sql, "./dbadmin", g:dbadmin_path, "")
    else
        let shellCmdStr=g:dbadmin_path
                    \ . " -s \"" . sql . "\""
                    \ . " -db ". g:dbadmin_db
                    \ . " -o " . g:dbadmin_output
                    \ . " -p " . g:dbadmin_page
                    \ . " -l " . g:dbadmin_limit
                    \ . (g:dbadmin_sql_no_cache == 'true' ? "" : " -c")
                    \ . (a:explain == 'false' ? "" : " -operation explain")
    endif
    echom shellCmdStr
    call s:RunShellCommand(shellCmdStr)
    " let sql=shellescape(sql)
    " execute '!_compile' shellescape(a:game) shellescape(a:major) shellescape(a:minor)
    " echom shellescape(shellCmdStr)
endfunction

" ################################ GetVisualSelection ################################
" xnoremap <leader><leader>v :<C-U> call GetVisualSelection(visualmode())<Cr>
function! GetVisualSelection(mode)
    " call with visualmode() as the argument
    let [line_start, column_start] = getpos("'<")[1:2]
    let [line_end, column_end]     = getpos("'>")[1:2]
    let lines = getline(line_start, line_end)
    if a:mode ==# 'v'
        " Must trim the end before the start, the beginning will shift left.
        let lines[-1] = lines[-1][: column_end - (&selection == 'inclusive' ? 1 : 2)]
        let lines[0] = lines[0][column_start - 1:]
    elseif  a:mode ==# 'V'
        " Line mode no need to trim start or end
    elseif  a:mode == "\<c-v>"
        " Block mode, trim every line
        let new_lines = []
        let i = 0
        for line in lines
            let lines[i] = line[column_start - 1: column_end - (&selection == 'inclusive' ? 1 : 2)]
            let i = i + 1
        endfor
    else
        return ''
    endif
    " for line in lines
    "     echom line
    " endfor
    return join(lines, "\n")
endfunction

" ################################ shellcmd ################################
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
  echo a:cmdline
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
     if part[0] =~ '\v[%#<]'
        let expanded_part = fnameescape(expand(part))
        let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
     endif
  endfor
  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, 'You entered:    ' . a:cmdline)
  call setline(2, 'Expanded Form:  ' .expanded_cmdline)
  call setline(3, substitute(getline(2),'.','=','g'))
  execute '$read !'. expanded_cmdline
  setlocal nomodifiable
  " 1
endfunction

" ################################ StartsWith ################################
fu! StartsWith(longer, shorter) abort
  return a:longer[0:len(a:shorter)-1] ==# a:shorter
endfunction
