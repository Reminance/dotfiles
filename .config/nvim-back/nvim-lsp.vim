augroup nvim_lsp
    au!
    au FileType java lua require'lspconfig/lspconfig_jdtls'.setup{}
    au FileType c,cpp,objc,objcpp lua require'lspconfig'.ccls.setup{}
    " au FileType go,gomod lua require'lspconfig'.gopls.setup{}
    au FileType go,gomod lua require'lspconfig/lspconfig_gopls'.setup{}
    au FileType rust lua require'lspconfig'.rls.setup{}
    au FileType python lua require'lspconfig'.pyls.setup{}
    au FileType sh lua require'lspconfig'.bashls.setup{}
    au FileType lua lua require'lspconfig'.sumneko_lua.setup{}
    au FileType html lua require'lspconfig'.html.setup{}
    au FileType json lua require'lspconfig'.jsonls.setup{}
augroup end

lua require'lspconfig'.gopls.setup{}

" Use completion-nvim in every buffer
autocmd BufEnter * lua require'completion'.on_attach()
autocmd BufWritePre *.go lua goimports(1000)

" nvim-completion settings
" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" " use <Tab> as trigger keys
" imap <Tab> <Plug>(completion_smart_tab)
" imap <S-Tab> <Plug>(completion_smart_s_tab)

" let g:completion_confirm_key = ""
" imap <expr> <CR>  pumvisible() ? complete_info()["selected"] != "-1" ?
"                  \ "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<CR>"

"map <C-Space> to manually trigger completion
imap <silent> <C-Space> <Plug>(completion_trigger)
"use <C-j> to switch to previous completion
" imap <C-j> <Plug>(completion_next_source)
"use <C-k> to switch to next completion
" imap <C-k> <Plug>(completion_prev_source)

" complete settings

" Set completeopt to have a better completion experience
" define the completeopt in init.vim
" set completeopt=menuone,noinsert,noselect
" Avoid showing message extra message when using completion
" set shortmess+=c

" complete style
    " \    {'complete_items': ['snippet', 'buffers', 'tags']},
    "
    " dbui is buggy, so comment below by now
    " \ 'sql': [
    " \    {'complete_items': ['vim-dadbod-completion']},
    " \    {'mode': '<C-p>'},
    " \    {'mode': '<C-n>'}
    " \],
let g:completion_chain_complete_list = {
    \ 'vim': [
    \    {'complete_items': ['snippet', 'tags']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'java': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'c': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'go': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'rust': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'python': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'sh': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'lua': [
    \    {'complete_items': ['snippet', 'lsp']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \],
    \ 'default': [
    \    {'complete_items': ['snippet']},
    \    {'mode': '<C-p>'},
    \    {'mode': '<C-n>'}
    \]
\}
let g:completion_enable_auto_popup = 1
let g:completion_auto_change_source = 1
let g:completion_enable_auto_hover = 1
let g:completion_enable_auto_signature = 1
" possible value: "length", "alphabet", "none"
let g:completion_sorting = "alphabet"
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']
" let g:completion_matching_ignore_case = 1
let g:completion_matching_smart_case = 1
let g:completion_trigger_keyword_length = 1 " default = 1
let g:completion_trigger_on_delete = 1
let g:completion_timer_cycle = 100 "default value is 80
augroup CompletionTriggerCharacter
    autocmd!
    autocmd BufEnter * let g:completion_trigger_character = ['.']
    autocmd BufEnter *.c,*.cpp let g:completion_trigger_character = ['.', '::']
augroup end

" possible value: 'UltiSnips', 'Neosnippet', 'vim-vsnip', 'snippets.nvim'
let g:completion_enable_snippet = 'UltiSnips'


" custom deprecated lua script
" nnoremap <Leader>ff :lua require'finder_config'.files()<CR>
" nnoremap <Leader>fl :lua require'finder_config'.fuzzy_grep()<CR>
" nnoremap <A-y>fl :lua require'finder'.help_tags()<CR>
" lua require'completion_config'
