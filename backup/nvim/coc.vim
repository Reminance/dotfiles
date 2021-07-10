
" coc
  " \ 'coc-sourcekit',
  " \ 'coc-leetcode',
let g:coc_global_extensions=[
  \ 'coc-sh',
  \ 'coc-clangd',
  \ 'coc-cmake',
  \ 'coc-rls',
  \ 'coc-java',
  \ 'coc-go',
  \ 'coc-pairs',
  \ 'coc-pyright',
  \ 'coc-python',
  \ 'coc-tslint',
  \ 'coc-tsserver',
  \ 'coc-flutter',
  \ 'coc-actions',
  \ 'coc-css',
  \ 'coc-diagnostic',
  \ 'coc-eslint',
  \ 'coc-git',
  \ 'coc-gitignore',
  \ 'coc-html',
  \ 'coc-json',
  \ 'coc-lists',
  \ 'coc-prettier',
  \ 'coc-snippets',
  \ 'coc-syntax',
  \ 'coc-tasks',
  \ 'coc-todolist',
  \ 'coc-translator',
  \ 'coc-vimlsp',
  \ 'coc-vimlsp',
  \ 'coc-yaml',
  \ 'coc-lists',
  \ 'coc-xml',
  \ 'coc-wxml',
  \ 'coc-restclient',
  \ 'coc-webpack',
  \ 'coc-pyls',
  \ 'coc-jedi',
  \ 'coc-db',
  \ 'coc-sql',
  \ 'coc-emoji',
  \ 'coc-floaterm',
  \ 'coc-emoji',
  \ 'coc-protobuf',
  \ 'coc-project-manager',
  \ 'coc-lua',
  \ 'coc-julia',
  \ 'coc-yank',
  \ 'coc-stylelint',
  \ 'coc-explorer',
  \ 'coc-marketplace']

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
" set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" use <tab> for trigger completion and navigate to the next complete item
inoremap <silent><expr> <TAB>
      \ pumvisible() ? coc#_select_confirm() :
      \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
function! s:check_back_space() abort
    let col=col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

inoremap <silent><expr> <C-Space> coc#refresh()
" use <,c,> to prev diagnostic;
nmap <silent> <Leader>c, <Plug>(coc-diagnostic-prev)
" use <,c.> to next diagnostic;
nmap <silent> <Leader>c. <Plug>(coc-diagnostic-next)
" nmap <silent> <Leader>c. <Plug>(coc-diagnostic-next-error)
inoremap <silent><expr> <S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <CR> could be remapped by other vim plugin
inoremap <silent><expr> <CR> pumvisible() ? coc#_select_confirm()
            \: "\<C-g>u\<CR>\<C-r>=coc#on_enter()\<CR>"

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Open up coc-commands
nnoremap <Leader>cC :CocCommand<CR>
" Text Objects
xmap kf <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap kf <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Useful commands
nnoremap <silent> <Leader>y :<C-u>CocList -A --normal yank<CR>

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gt <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <Leader>rn <Plug>(coc-rename)
nnoremap <Leader>tt :CocCommand explorer<CR>

nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction

" coc-translator
nmap <C-Y> <Plug>(coc-translator-p)
vmap <C-Y> <Plug>(coc-translator-pv)

" " Remap for do codeAction of selected region
" function! s:cocActionsOpenFromSelected(type) abort
"   execute 'CocCommand actions.open ' . a:type
" endfunction
" xmap <silent> <Leader>a :<C-u>execute 'CocCommand actions.open ' . visualmode()<CR>
" nmap <silent> <Leader>a :<C-u>set operatorfunc=<SID>cocActionsOpenFromSelected<CR>g@

" Applying codeAction to the selected region.
" Example: `<Leader>aap` for current paragraph
xmap <M-CR> <Plug>(coc-codeaction-selected)
" nmap <Leader>cs  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <M-CR> <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <Leader>cq  <Plug>(coc-fix-current)

" coctodolist
nnoremap <Leader>tn :CocCommand todolist.create<CR>
nnoremap <Leader>tl :CocList todolist<CR>
nnoremap <Leader>tu :CocCommand todolist.download<CR>:CocCommand todolist.upload<CR>
" coc-tasks
nnoremap <Leader>ts :CocList tasks<CR>

" coc-snippets
" Use <C-l> for trigger snippet expand.
imap <C-l> <Plug>(coc-snippets-expand)
" Use <C-j> for select text for visual placeholder of snippet.
vmap <C-j> <Plug>(coc-snippets-select)
" 直接展开snippets 如果有lsp的补全也会跳过， 直接展开snippetes的候选
" Use <C-j> for both expand and jump (make expand higher priority.)
imap <C-\> <Plug>(coc-snippets-expand-jump)
" Use <leader>x for convert visual selected code to snippet
xmap <Leader>x  <Plug>(coc-convert-snippet)
let g:snips_author='Reminance'
" 在snippets占位符间跳转
let g:coc_snippet_next='<C-j>'
let g:coc_snippet_prev='<C-k>'

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <Leader>cld  :<C-u>CocList diagnostics<CR>
" Manage extensions.
nnoremap <silent><nowait> <Leader>cle  :<C-u>CocList extensions<CR>
" Show commands.
nnoremap <silent><nowait> <Leader>clc  :<C-u>CocList commands<CR>
" Find symbol of current document.
nnoremap <silent><nowait> <Leader>clo  :<C-u>CocList outline<CR>
" Search workspace symbols.
nnoremap <silent><nowait> <Leader>cls  :<C-u>CocList -I symbols<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <Leader>clr  :<C-u>CocListResume<CR>
" marketplace coc list.
nnoremap <silent><nowait> <Leader>clm  :<C-u>CocList marketplace<CR>
" Do default action for next item.
nnoremap <silent><nowait> <Leader>cn  :<C-u>CocNext<CR>
" Do default action for previous item
nnoremap <silent><nowait> <Leader>cp  :<C-u>CocPrev<CR>

" ===
" === coc-leetcode
" ===
" let g:leetcode_language='java'
" let g:leetcode_trace_server=0
" let g:leetcode_enabled=1
" nnoremap <silent> <Leader>ll :CocCommand leetecode.login<CR>
" nnoremap <silent> <Leader>lp :CocList LeetcodeProblems<CR>
" nnoremap <silent> <Leader>lr :CocCommand leetcode.run<CR>
" nnoremap <silent> <Leader>ls :CocCommand leetcode.submit<CR>
" nnoremap <silent> <Leader>lc :CocCommand leetcode.comments<CR>

" ===
" === coc-restclient
" ===
nnoremap <Leader>crr :CocCommand rest-client.request<CR>
nnoremap <Leader>crl :r! cat ~/.config/coc/extensions/node_modules/coc-restclient/test/sample/single.http<CR>
nnoremap <Leader>crL :r! cat ~/.config/coc/extensions/node_modules/coc-restclient/test/sample/multiple.http<CR>

" go coc settings
autocmd BufWritePre *.go :call CocAction('runCommand', 'editor.action.organizeImport')

