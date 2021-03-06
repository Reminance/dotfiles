" Vim-plug Auto Load
" Auto load for the first time
if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
    echo "Downloading junegunn/vim-plug to manage plugins..."
    silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

" Plug Install
call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))

" status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
" Plug 'itchyny/lightline.vim'

" themes
Plug 'connorholyday/vim-snazzy'
Plug 'morhetz/gruvbox'
Plug 'w0ng/vim-hybrid'
Plug 'whatyouhide/vim-gotham'

" vim-peekaboo
" Peekaboo extends " and @ in normal mode and <CTRL-R> in insert mode
" so you can see the contents of the registers.
Plug 'junegunn/vim-peekaboo'

" File navigation
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'

" fzf
Plug 'junegunn/fzf.vim'

" ranger
Plug 'francoiscabrol/ranger.vim'

" Tagbar
Plug 'majutsushi/tagbar', { 'on': 'TagbarOpenAutoClose' }

" Error checking
"Plug 'w0rp/ale'

" Undo Tree
"Plug 'mbbill/undotree/'

" Other visual enhancement
" Plug 'nathanaelkane/vim-indent-guides'
Plug 'Yggdroot/indentLine'
" Plug 'itchyny/vim-cursorword'

" Git
" Plug 'rhysd/conflict-marker.vim'
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
" Plug 'airblade/vim-gitgutter'
" Plug 'gisphm/vim-gitignore', { 'for': ['gitignore', 'vim-plug'] }

" HTML, CSS, JavaScript, PHP, JSON, etc.
" Plug 'elzr/vim-json'
" Plug 'hail2u/vim-css3-syntax'
" merge of ap vim-css-color and colorizer. The main goal was to fix cursorline bug and keep named colors(i.e. white, black, aqua).
Plug 'gko/vim-coloresque', { 'for': ['vim-plug', 'php', 'html', 'javascript', 'css', 'less'] }
" Plug 'pangloss/vim-javascript', { 'for' :['javascript', 'vim-plug'] }
" Plug 'mattn/emmet-vim'

" Snippets
" Track the engine.
Plug 'SirVer/ultisnips'
" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'

" Python
" Plug 'vim-scripts/indentpython.vim'

" Go
" Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }

" Markdown
" Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'dhruvasagar/vim-table-mode', { 'on': 'TableModeToggle' }
"Plug 'vimwiki/vimwiki'

" Plug 'easymotion/vim-easymotion'

" Find & Replace
Plug 'brooth/far.vim', { 'on': ['F', 'Far', 'Fardo'] }

" Bookmarks
Plug 'MattesGroeger/vim-bookmarks'

" Other visual enhancement
Plug 'mhinz/vim-startify'
Plug 'ryanoasis/vim-devicons'
" Plug 'luochen1990/rainbow'
" Plug 'wincent/terminus'

" A Vim Plugin for Lively Previewing LaTeX PDF Output
" Plug 'xuhdev/vim-latex-live-preview', { 'for': 'tex' }

" Other useful utilities
" Plug 'makerj/vim-pdf'
" Plug 'mg979/vim-visual-multi', {'branch': 'master'}
Plug 'tpope/vim-surround' " type ysks' to wrap the word with '' or type cs'` to change 'word' to `word`
" Plug 'godlygeek/tabular' " type ;Tabularize /= to align the =
" Plug 'gcmt/wildfire.vim' " in Visual mode, type i' to select all text in '', or type i) i] i} ip
Plug 'tpope/vim-commentary' " gcc to comment the current line

" Dependencies
" Plug 'MarcWeber/vim-addon-mw-utils'
" Plug 'kana/vim-textobj-user'
" Plug 'fadein/vim-FIGlet'

" Debugger
" Plug 'puremourning/vimspector', {'do': './install_gadget.py --enable-c --enable-python --enable-go'}

" which-key
Plug 'liuchengxu/vim-which-key'

" floaterm
Plug 'voldikss/vim-floaterm'

" auto-pairs
" Plug 'jiangmiao/auto-pairs'

" vista.vim
" Plug 'liuchengxu/vista.vim'

"ianding1/leetcode.vim
" Plug 'ianding1/leetcode.vim'

" vim-easy-align
Plug 'junegunn/vim-easy-align'

" tpope/vim-dispatch
Plug 'tpope/vim-dispatch'

" voldikss/vim-translator
Plug 'voldikss/vim-translator'

" code complete
" Plug 'neoclide/coc.nvim', {'branch': 'release'}

" neovim/nvim-lspconfig
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
" Plug 'nvim-lua/completion-nvim'
" Plug 'nvim-lua/popup.nvim'
" Plug 'mfussenegger/nvim-jdtls'
" Plug 'steelsojka/completion-buffers'
" Plug 'kristijanhusak/completion-tags'
" Plug 'nvim-lua/plenary.nvim'
" Plug 'nvim-telescope/telescope.nvim'
" Plug 'bfredl/nvim-luadev'

" fcitx optimization
" Plug 'lilydjwg/fcitx.vim'

Plug 'junegunn/goyo.vim'
" Plug 'jreybert/vimagit'

" Plug 'tweekmonster/startuptime.vim'

call plug#end()

" Plug Settings
" latex
" compile to pdf
" arch requirements sudo pacman -S texlive-most texlive-lanf mupdf zathura
nnoremap \l :! pdflatex %<CR><CR>
" view the pdf
nnoremap \p :! mupdf $(echo % \| sed 's/tex$/pdf/') & disown<CR><CR>
" delete the compling output
nnoremap \c :! ls \| grep -E '*.aux\|*.log\|*.nav\|*.out\|*.snm\|*.toc\|*.pdf' \| xargs rm -rf {}<CR><CR>

" vim-latex-live-preview
" let g:livepreview_previewer='zathura'
" let g:livepreview_previewer='mupdf'
" let g:livepreview_engine='pdflatex'

" airline
" let g:airline_theme='onedark'
" let g:airline_theme='papercolor'
" let g:airline_theme='codedark'
let g:airline_theme='molokai'

" lightline
" NearestMethodOrFunction cames from vista function below
" \ 'colorscheme': 'wombat',
" \ 'colorscheme': 'gotham',
" \ 'colorscheme': 'PaperColor',
"
" \ 'component_function': {
" \   'method': 'NearestMethodOrFunction'
" \ },
let g:lightline={
            \ 'colorscheme': 'jellybeans',
            \ 'active': {
                \   'left': [ [ 'mode', 'paste' ],
                \             [ 'readonly', 'filename', 'modified', 'method' ] ]
                \ },
                \ }

" colorscheme
let g:SnazzyTransparent=1
colorscheme snazzy
" colorscheme gotham
" colorscheme gruvbox
" colorscheme hybrid

" vim-peekaboo
" suppress the default key bindings if you need imap <C-r> keybinding
" let g:peekaboo_ins_prefix='<S>'

" NERDTree
nnoremap tt :NERDTreeToggle<CR>
"let NERDTreeMapOpenExpl=""
"let NERDTreeMapUpdir=""
"let NERDTreeMapUpdirKeepOpen=""
"let NERDTreeMapOpenSplit=""
"let NERDTreeOpenVSplit=""
"let NERDTreeMapActivateNode=""
"let NERDTreeMapOpenInTab=""
"let NERDTreeMapPreview=""
"let NERDTreeMapCloseDir=""
"let NERDTreeMapChangeRoot=""

" NERDTree-git
let g:NERDTreeGitStatusIndicatorMapCustom={
            \ "Modified"  : "???",
            \ "Staged"    : "???",
            \ "Untracked" : "???",
            \ "Renamed"   : "???",
            \ "Unmerged"  : "???",
            \ "Deleted"   : "???",
            \ "Dirty"     : "???",
            \ "Clean"     : "??????",
            \ "Unknown"   : "?"
            \ }

" wildfire
" " This selects the next closest text object.
" nnoremap <CR> <plug>(wildfire-fuel)
" " this selects the previous closest text object.
" " vmap <C-Space> <plug>(wildfire-water)
" let g:wildfire_objects=["i'", 'i"', "i>", "i)", "i]", "i}", "ip", "it"]

" ale
"let b:ale_linters=['pylint']
"let b:ale_fixers=['autopep8', 'yapf']

" Tagbar
" Tagbar might need sudo pacman -S ctags
nnoremap <silent> T :TagbarOpenAutoClose<CR>

" Ultisnips
" Trigger configuration. You need to change this to something other than <tab> if you use one of the following:
" - https://github.com/Valloric/YouCompleteMe
" - https://github.com/nvim-lua/completion-nvim
let g:UltiSnipsExpandTrigger='<C-\>'
let g:UltiSnipsJumpForwardTrigger='<C-j>'
let g:UltiSnipsJumpBackwardTrigger='<C-k>'
let g:UltiSnipsSnippetDirectories=["~/.config/nvim/Ultisnips"]
" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

function! s:edit_snippets(snippets_name)
    exe 'vsp ~/.config/nvim/Ultisnips/'.a:snippets_name
endfunction
command! -bang -nargs=* EditUtilSnips call fzf#run({
            \ 'source': 'ls -1 ~/.config/nvim/Ultisnips',
            \   'down': 20,
            \   'sink': function('<sid>edit_snippets')
            \ })

" MarkdownPreview
let g:mkdp_auto_start=0
let g:mkdp_auto_close=1
let g:mkdp_refresh_slow=0
let g:mkdp_command_for_global=0
let g:mkdp_open_to_the_world=0
let g:mkdp_open_ip=''
let g:mkdp_browser='google-chrome-stable'
let g:mkdp_echo_preview_url=1
let g:mkdp_browserfunc=''
let g:mkdp_preview_options={
            \ 'mkit': {},
            \ 'katex': {},
            \ 'uml': {},
            \ 'maid': {},
            \ 'disable_sync_scroll': 0,
            \ 'sync_scroll_type': 'middle',
            \ 'hide_yaml_meta': 1
            \ }
let g:mkdp_markdown_css=''
let g:mkdp_highlight_css=''
let g:mkdp_port=''
let g:mkdp_page_title='???${name}???'

" vim-table-mode
nnoremap <Leader>tm :TableModeToggle<CR>

" vim-easymotion
" let g:EasyMotion_do_mapping=0
" let g:EasyMotion_do_shade=0
" let g:EasyMotion_smartcase=1
" nmap <Leader>e <Plug>(easymotion-bd-f2)

" Far.vim
nnoremap <Leader>fa :F  %<left><left>

" fzf.vim
nnoremap <M-S-l> :Lines<CR>
" ripgrep
nnoremap <M-S-f> :Rg<CR>
" The Silver Searcher
nnoremap <M-S-a> :Ag<CR>
nnoremap <M-S-g> :GFiles<CR>
nnoremap <M-S-d> :GFiles?<CR>
nnoremap <M-S-n> :Files<CR>
nnoremap <M-S-e> :Buffers<CR>
nnoremap <M-S-h> :History<CR>
nnoremap <M-S-t> :BTags<CR>
nnoremap <M-S-c> :BCommits<CR>
let g:fzf_preview_window='right:60%'
let g:fzf_commits_log_options='--graph --color=always --format="%C(auto)%h%d %s %C(blue)%C(bold)%cr"'
" nnoremap <M-S-c> :Commits<CR>

" ranger
" suppress the default key bindings
let g:ranger_map_keys=''
nnoremap <Leader>ra :Ranger<CR>

" vim-indent-guide
" " suppress the default key bindings
" " autocmd WinEnter * silent! unmap ,ig
" nmap <silent> <Leader><Leader>ig <Plug>IndentGuidesToggle
" let g:indent_guides_enable_on_vim_startup=1
" let g:indent_guides_guide_size=1
" let g:indent_guides_start_level=2
" let g:indent_guides_color_change_percent=1
" " autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=red   ctermbg=3
" " autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=green ctermbg=4

" indentLine
let g:indentLine_char_list = ['|', '??', '???', '???']

" Undotree
"map P :UndotreeToggle<CR>
"let g:undotree_DiffAutoOpen=1
"let g:undotree_SetFocusWhenToggle=1
"let g:undotree_ShortIndicators=1
"let g:undotree_WindowLayout=2
"let g:undotree_DiffpanelHeight=8
"let g:undotree_SplitWidth=24
"function g:Undotree_CustomMap()
"nmap <buffer> j <plug>UndotreeNextState
"nmap <buffer> k <plug>UndotreePreviousState
"nmap <buffer> J 5<plug>UndotreeNextState
"nmap <buffer> K 5<plug>UndotreePreviousState
"endfunc

" rainbow
let g:rainbow_active=1

" vim-bookmarks
let g:bookmark_no_default_key_mappings=1
nmap <Leader>mt <Plug>BookmarkToggle
nmap <Leader>ma <Plug>BookmarkAnnotate
nmap <Leader>ml <Plug>BookmarkShowAll
nmap <Leader>mn <Plug>BookmarkNext
nmap <Leader>mp <Plug>BookmarkPrev
nmap <Leader>mc <Plug>BookmarkClear
nmap <Leader>mx <Plug>BookmarkClearAll
nmap <Leader>mk <Plug>BookmarkMoveUp
nmap <Leader>mj <Plug>BookmarkMoveDown
nmap <Leader>mg <Plug>BookmarkMoveToLine
let g:bookmark_save_per_working_dir=1
let g:bookmark_auto_save=1
let g:bookmark_highlight_lines=1
let g:bookmark_manage_per_buffer=1
let g:bookmark_save_per_working_dir=1
let g:bookmark_center=1
let g:bookmark_auto_close=1
let g:bookmark_location_list=1

" vim-startify
let g:startify_custom_header='startify#pad(startify#fortune#cowsay())'
" let g:startify_custom_header='startify#pad(["REMINANCE"])'
" let g:startify_custom_header=[]
let g:startify_session_autoload=1
let g:startify_session_dir='~/.config/nvim/session'
let g:startify_lists=[
            \ { 'type': 'files',     'header': ['   Files']            },
            \ { 'type': 'dir',       'header': ['   Current Directory '. getcwd()] },
            \ { 'type': 'sessions',  'header': ['   Sessions']       },
            \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
            \ { 'type': 'commands',  'header': ['   Commands']       },
            \ ]
let g:startify_bookmarks=[
            \ { 'c': '~/.config/i3/config' },
            \ { 'i': '~/.config/nvim/init.vim' },
            \ { 'z': '~/.zshrc' },
            \ '~/workspace',
            \ '~/Downloads',
            \ '~/picture',
            \ ]

" Open Startify
nnoremap <Leader>\ :Startify<CR>
nnoremap <Leader>ss :SSave! Session.session<CR>
nnoremap <Leader>sS :SSave!
nnoremap <Leader>sl :SLoad! Session.session<CR>
nnoremap <Leader>sL :SLoad!
nnoremap <Leader>sd :SDelete!<CR>
nnoremap <Leader>sc :SClose<CR>

" vim-surround
" cs"'    cs'</q>    cst"    ds"    ysiw]    yss)    yss"
" suppress the default key bindings
let g:surround_no_insert_mappings=1

" vim-fugitive
nnoremap <Leader>gb :Git blame<CR>
nnoremap <Leader>gc :G commit<CR>
nnoremap <Leader>gf :Git fetch<CR>
nnoremap <Leader>gF :Git pull<CR>
nnoremap <Leader>gp :Git push<CR>
nnoremap <Leader>gd :Gvdiffsplit<CR>
nnoremap <Leader>gh :diffget //2<CR>
nnoremap <Leader>gl :diffget //3<CR>
nnoremap <Leader>G :G<CR>

" vim-signify
" ??????????????????VCS
let g:signify_vcs_list=['git']
" ?????????????????????updatetime??????????????????????????????????????????????????????
let g:signify_cursorhold_insert=1
" ?????????????????????updatetime??????????????????????????????????????????????????????
let g:signify_cursorhold_normal=1
" ?????????????????????????????????
let g:signify_update_on_bufenter=0
" vim???????????????????????????
let g:signify_update_on_focusgained=1
" ????????????
nnoremap <Leader>gsd :SignifyDiff<CR>
nnoremap <Leader>gst :SignifyToggle<CR>
nnoremap <Leader>gsh :SignifyToggleHighlight<CR>
nnoremap <Leader>gsr :SignifyRefresh<CR>
nnoremap <Leader>gsx :SignifyDebug<CR>
" hunk jumping
nmap <Leader>gsn <plug>(signify-next-hunk)
nmap <Leader>gsp <plug>(signify-prev-hunk)

" GitGutter
""let g:gitgutter_signs=0
"let g:gitgutter_map_keys=0
"let g:gitgutter_override_sign_column_highlight=1
""let g:gitgutter_highlight_linenrs=1
"let g:gitgutter_preview_win_floating=1
"autocmd BufWritePost * GitGutter
"nnoremap <Leader>gf :GitGutterFold<CR>
"nnoremap <Leader>gh :GitGutterPreviewHunk<CR>
"nnoremap <Leader>g[ :GitGutterPrevHunk<CR>
"nnoremap <Leader>g] :GitGutterNextHunk<CR>

" vimspector
" let g:vimspector_enable_mappings='HUMAN'
" let g:vimspector_install_gadgets=[ 'vscode-cpptools', 'debugpy', 'vscode-go' ]
" " let g:vimspector_base_dir=expand( '$HOME/.config/nvim/vimspector-config' )

" ??????????????????????????????buffer
" function! s:read_template_into_buffer(template)
"     " has to be a function to avoid the extra space fzf#run insers otherwise
"     execute '0r ~/.config/nvim/vimspector-config/'.a:template
" endfunction
" command! -bang -nargs=* LoadVimSpectorJsonTemplate call fzf#run({
"             \   'source': 'ls -1 ~/.config/nvim/vimspector-config',
"             \   'down': 20,
"             \   'sink': function('<sid>read_template_into_buffer')
"             \ })

" " nnoremap <Leader>vs :tabe .vimspector.json<CR>:LoadVimSpectorJsonTemplate<CR>
" sign define vimspectorBP text=??? texthl=Normal
" sign define vimspectorBPDisabled text=??? texthl=Normal
" sign define vimspectorPC text=???? texthl=SpellBad

" which-key
" let g:mapleader="\<Space>"
" nnoremap <silent> <Leader>      :<C-u>WhichKey '<Space>'<CR>
let g:mapleader=','
let g:maplocalleader=','
nnoremap <silent> <Leader>      :<C-u>WhichKey ','<CR>
nnoremap <silent> <LocalLeader> :<C-u>WhichKey  ','<CR>

" floaterm
let g:floaterm_keymap_toggle='<Leader>.'
let g:floaterm_keymap_next='<Leader>f.'
let g:floaterm_keymap_prev='<Leader>f,'
let g:floaterm_keymap_new='<Leader>f+'
" let g:floaterm_keymap_hide='<Leader>fh'
" let g:floaterm_keymap_show='<Leader>fs'
let g:floaterm_keymap_kill='<Leader>fq'
let g:floaterm_opener='edit'
let g:floaterm_autoinsert=1
let g:floaterm_width=0.6
let g:floaterm_height=0.6
" let g:floaterm_autoclose=1
nnoremap <silent> <Leader>; :FloatermNew --wintype=popup --height=6<CR>
nnoremap <silent> <Leader>ff :FloatermNew fzf<CR>
nnoremap <silent> <Leader>fg :FloatermNew lazygit<CR>
nnoremap <silent> <Leader>fn :FloatermNew node<CR>
nnoremap <silent> <Leader>fp :FloatermNew python<CR>
nnoremap <silent> <Leader>fh :FloatermNew htop<CR>
nnoremap <silent> <Leader>fd :FloatermNew ncdu<CR>

" auto-pairs
" let g:AutoPairsShortcutToggle='<Leader>apt'
" let g:AutoPairsShortcutFastWrap='<Leader>apf'
" let g:AutoPairsShortcutJump='<Leader>apj'
" let g:AutoPairsShortcutBackInsert='<Leader>api'

" vista
" function! NearestMethodOrFunction() abort
"     return get(b:, 'vista_nearest_method_or_function', '')
" endfunction
" set statusline+=%{NearestMethodOrFunction()}

" By default vista.vim never run if you don't call it explicitly.
"
" If you want to show the nearest function in your statusline automatically,
" you can add the following line to your vimrc
" autocmd VimEnter * call vista#RunForNearestMethodOrFunction()

" nnoremap <Leader>vt :Vista!!<CR>
" " nnoremap <Leader>vf :silent! Vista finder coc<CR>
" let g:vista_icon_indent=["????????? ", "????????? "]
" " let g:vista_default_executive='coc'
" let g:vista_default_executive='ctags'
" " Set the executive for some filetypes explicitly. Use the explicit executive
" " instead of the default one for these filetypes when using `:Vista` without
" " specifying the executive.
" " let g:vista_executive_for = {
" "   \ 'cpp': 'vim_lsp',
" "   \ 'php': 'vim_lsp',
" "   \ }
" let g:vista_fzf_preview=['right:50%']
" let g:vista#renderer#enable_icon=1
" let g:vista#renderer#icons={
" \   "function": "\uf794",
" \   "variable": "\uf71b",
" \  }

" leetcode.vim
" let g:leetcode_china=1
" " Values: 'cpp', 'java', 'python', 'python3', 'csharp', 'javascript', 'ruby', 'swift', 'golang', 'scala', 'kotlin', 'rust'. Default:'cpp'.
" let g:leetcode_solution_filetype='java'
" let g:leetcode_browser='chrome'
" " let g:leetcode_browser='chromium'
" nnoremap <Leader>ll :LeetCodeList<CR>
" nnoremap <Leader>lt :LeetCodeTest<CR>
" nnoremap <Leader>ls :LeetCodeSubmit<CR>
" nnoremap <Leader>li :LeetCodeSignIn<CR>

" vim-easy-align
" Start interactive EasyAlign in visual mode (e.g. vipga); custom:vip<Leader>ea
xmap <Leader>ea <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip); custom:<Leader>eaip
nmap <Leader>ea <Plug>(EasyAlign)

" vim-dispatch
" tpope/vim-dispatch
" By default dispatch.vim provides `<CR> for :Dispatch<CR>. You can find all
" default maps under :h dispatch-maps
" autocmd FileType java let b:dispatch = 'javac % && java %<'
" autocmd FileType cucumber compiler cucumber | setl makeprg=cucumber\ \"%:p\"
" autocmd FileType ruby
"     \ if expand('%') =~# '_test\.rb$' |
"     \   compiler rubyunit | setl makeprg=testrb\ \"%:p\" |
"     \ elseif expand('%') =~# '_spec\.rb$' |
"     \   compiler rspec | setl makeprg=rspec\ \"%:p\" |
"     \ else |
"     \   compiler ruby | setl makeprg=ruby\ -wc\ \"%:p\" |
"     \ endif
" autocmd User Bundler
"     \ if &makeprg !~# 'bundle' | setl makeprg^=bundle\ exec\  | endif
let g:dispatch_compilers = {
            \ 'latex': 'tex',
            \ 'bundle exec': '',
            \ 'java': 'javac % && java %<'
            \}
" open the quickfix from vim-dispatch
nnoremap <Leader>Co :Copen<CR>

" voldkiss/vim-translator settings

" coc.nvim
" source ~/.config/nvim/coc.vim

" Goyo plugin makes text more readable when writing prose:
map <leader>F :Goyo \| set bg=dark \| set linebreak<CR>

" telescope start
" Find files using Telescope command-line sugar.
" nnoremap <leader>ff <cmd>Telescope find_files<cr>
" nnoremap <leader>fg <cmd>Telescope live_grep<cr>
" nnoremap <leader>fb <cmd>Telescope buffers<cr>
" nnoremap <leader>fh <cmd>Telescope help_tags<cr>

" Using Lua functions
" nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
" nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
" nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
" nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
" telescope end

" nvim-lspconfig.nvim
" source ~/.config/nvim/nvim-lsp.vim

"" from completion-nvim start -->
"" Use <Tab> and <S-Tab> to navigate through popup menu
"inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
"inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
"" Set completeopt to have a better completion experience
"set completeopt=menuone,noinsert,noselect
"" Avoid showing message extra message when using completion
"set shortmess+=c
"" let g:completion_enable_auto_popup = 0
""map <c-p> to manually trigger completion
"" imap <silent> <c-p> <Plug>(completion_trigger)
"" Or you want to use <Tab> as trigger keys
"imap <tab> <Plug>(completion_smart_tab)
"imap <s-tab> <Plug>(completion_smart_s_tab)
"" By default other snippets source support are disabled, turn them on by
"" possible value: 'UltiSnips', 'Neosnippet', 'vim-vsnip', 'snippets.nvim'
"let g:completion_enable_snippet = 'UltiSnips'
"" By default <CR> is used to confirm completion and expand snippets, change it by
"" let g:completion_confirm_key = "\<C-y>"
"" If the confirm key has a fallback mapping, for example when using the auto pairs plugin, it maps to <CR>. You can avoid using the default confirm key option and use a mapping like this instead.
"" let g:completion_confirm_key = ""
"" imap <expr> <cr>  pumvisible() ? complete_info()["selected"] != "-1" ?
""                  \ "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<CR>"
"let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
"let g:completion_matching_smart_case = 1
"augroup CompletionTriggerCharacter
"    autocmd!
"    autocmd BufEnter * let g:completion_trigger_character = ['.']
"    autocmd BufEnter *.java,*.c,*.cpp let g:completion_trigger_character = ['.', '::']
"augroup end
"let g:completion_trigger_keyword_length = 2 " default = 1
"let g:completion_trigger_on_delete = 1
"" from completion-nvim end <--

command! LspLog execute 'lua vim.cmd("e"..vim.lsp.get_log_path())'
command! LspLogPrint execute 'lua print(vim.lsp.get_log_path())'

lua << EOF
local nvim_lsp = require('lspconfig')
vim.lsp.set_log_level("debug")

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
-- require('completion').on_attach()
local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

--Enable completion triggered by <c-x><c-o>
buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

-- Mappings.
local opts = { noremap=true, silent=true }

-- See `:help vim.lsp.*` for documentation on any of the below functions
buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- local servers = {'bashls', 'gopls', 'vimls', 'rust_analyzer', 'clangd', 'jdtls', 'sumneko_lua', 'pyright', 'tsserver', 'html', 'jsonls', 'cssls'}
local servers = {'bashls', 'gopls', 'vimls', 'rust_analyzer', 'clangd', 'pyright', 'html', 'jsonls', 'cssls'}
for _, lsp in ipairs(servers) do
    nvim_lsp[lsp].setup {
        on_attach = on_attach,
        flags = {
            debounce_text_changes = 150,
            }
        }
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    -- This will disable virtual text, like doing:
    -- let g:diagnostic_enable_virtual_text = 0
    virtual_text = true,

    -- This is similar to:
    -- let g:diagnostic_show_sign = 1
    -- To configure sign display,
    --  see: ":help vim.lsp.diagnostic.set_signs()"
    signs = false,

    -- This is similar to:
    -- "let g:diagnostic_insert_delay = 1"
    update_in_insert = false,
  }
)
EOF

command! -buffer -nargs=0 LspShowLineDiagnostics lua require'jumpLoc'.openLineDiagnostics()
nnoremap <buffer><silent> <C-h> <cmd>LspShowLineDiagnostics<CR>
command! Format execute 'lua vim.lsp.buf.formatting()'

" nvim-compe start
set completeopt=menuone,noselect
let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.debug = v:false
let g:compe.min_length = 1
let g:compe.preselect = 'enable'
let g:compe.throttle_time = 80
let g:compe.source_timeout = 200
let g:compe.resolve_timeout = 800
let g:compe.incomplete_delay = 400
let g:compe.max_abbr_width = 100
let g:compe.max_kind_width = 100
let g:compe.max_menu_width = 100
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.calc = v:true
let g:compe.source.nvim_lsp = v:true
let g:compe.source.nvim_lua = v:true
let g:compe.source.vsnip = v:false
let g:compe.source.ultisnips = v:true
let g:compe.source.luasnip = v:true
let g:compe.source.emoji = v:true

inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
" inoremap <silent><expr> <C-e>     compe#close('<C-e>')
" inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
" inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
" nvim-compe end
