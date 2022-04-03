-- General config

-- Stop comments on newline
-- vim.cmd [[autocmd BufWinEnter * :set formatoptions-=c formatoptions-=r formatoptions-=o]]

-- Disable lualine on Nvim-tree
-- vim.cmd [[au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif]]

-- go to last location when opening a buffer
-- vim.cmd [[ autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g`\"" | endif ]]

-- options --
vim.cmd [[
set nocompatible
let mapleader=" "
set shiftwidth=2 tabstop=4 softtabstop=4 expandtab smarttab autoindent smartindent
set ignorecase smartcase incsearch showmatch hlsearch
set wrap formatoptions-=t "turn off Auto-wrap text using textwidth
set scrolloff=2
set noshowmode
" set shortmess+=c
set nu rnu
set mouse=a
set inccommand=split
set hidden
" set autochdir " auto change cwd
" Save when losing focus
au FocusLost * :silent! wall
" Resize splits when the window is resized
au VimResized * :wincmd =
" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800
set backspace=indent,eol,start " set backspace=indent,eol,start; eol让退格键可以退到上一行

set completeopt=menuone,noinsert,noselect
" set signcolumn=yes
set updatetime=100
set encoding=UTF-8
set clipboard+=unnamedplus " Copy paste between vim and everything else
set nojoinspaces " don't autoinsert two spaces after '.', '?', '!' for join command
" set showcmd " extra info at end of command line
" set wildignore+=*/node_modules/**
filetype plugin indent on

" syntax highlighting of search results
au ColorScheme * highlight Search guibg=NONE guifg=Cyan gui=italic,underline,bold
" highlight Comment cterm=italic gui=italic

" Make it obvious where 80 characters is
set textwidth=80
" 80字符竖线指示
" set colorcolumn=+1
" set colorcolumn=80
" highlight ColorColumn guibg=#181818

" Stop comments on newline
autocmd BufWinEnter * :set formatoptions-=c formatoptions-=r formatoptions-=o

" Disable lualine on Nvim-tree
au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if bufname('%') == "NvimTree" | set laststatus=0 | else | set laststatus=2 | endif

" Line Return
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif " make cursor remain the position of last quit

" Backups
set backup                        " enable backups
set noswapfile
silent !mkdir -p ~/.config/nvim/tmp/backup
silent !mkdir -p ~/.config/nvim/tmp/undo
"silent !mkdir -p ~/.config/nvim/tmp/sessions
set backupdir=~/.config/nvim/tmp/backup,.
set directory=~/.config/nvim/tmp/backup,.
" use for u(undo) and Ctrl-r(redo)
if has('persistent_undo')
    set undofile
    set undodir=~/.config/vim/tmp/undo,.
endif

" status line
set laststatus=0  " disable status line
" set showtabline=0 " disable tab line
function! ToggleHiddenStatusLine()
    if &laststatus < 2
        set laststatus=2
        " set showtabline=2
    else
        set laststatus=0
        " set showtabline=0
    endif
endfunction
nnoremap <Leader>. :call ToggleHiddenStatusLine()<CR>

" general key mappings
inoremap <C-g> <Esc>
map <C-g> <Esc>

" Don't move on *
nnoremap <silent> * mm*`m
" Visual Mode */# from Scrooloose
function! s:VSetSearch()
    let temp = @@
    norm! gvy
    let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
    let @@ = temp
endfunction
vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>

" Save & quit
nnoremap s <nop>
nnoremap R <nop>
nnoremap Q <nop>
nnoremap <C-q> :q<CR>
inoremap <C-q> <Esc>:q<CR>
inoremap <C-s> <Esc>:w<CR>
nnoremap <C-s> :w<CR>

" Source
nnoremap <M-s> <nop>
nnoremap <M-s>i :source $MYVIMRC<CR>
nnoremap <M-s>. :so %<CR>
vnoremap <M-s>v y:execute @@<CR>:echo 'Sourced selection.'<CR>
nnoremap <M-s>L ^vg_y:execute @@<CR>:echo 'Sourced line.'<CR>

" Save file as sudo on files that require root permission(by typing [Ctrl-s !]), note the '' symbol is type through Ctrl-v ctrl-s
cnoremap ! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" normal mode bindings
nnoremap <Down> :res +1<CR>
nnoremap <Up> :res -1<CR>
nnoremap <Left> :vertical resize-1<CR>
nnoremap <Right> :vertical resize+1<CR>

" Basic Mappings
nnoremap <Leader><Space> :nohlsearch<CR>

" insert a <++>
inoremap <M-i> <++>
" jump to next <++> and replace it
nnoremap <M-Space> <Esc>/<++><CR>:nohlsearch<CR>c4l
inoremap <M-Space> <Esc>/<++><CR>:nohlsearch<CR>c4l

" move selected lines up/down and keep selected
vnoremap J :move '>+1<CR>gv=gv
vnoremap K :move '<-2<CR>gv=gv
" indent blocks and keep selected
vnoremap H <gv
vnoremap L >gv
" inoremap <C-J> <Esc>mz:m .+1<CR>==`za
" inoremap <C-K> <Esc>mz:m .-2<CR>==`za

" copy from rigister to the vim cmdline
nnoremap <Leader>cp :<C-r>"
" Clean trailing whitespace
nnoremap <Leader>ww mz:%s/\s\+$//<cr>:let @/=''<CR>`z
" Panic Button
nnoremap <M-x>e mzggg?G`z
" Rot13 encode {motion} text.
vnoremap <M-x>e mzg?`z
" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap <Leader>vv ^vg_
" Perform dot commands over visual blocks:
vnoremap . :normal .<CR>
" Replace all is aliased to R.
nnoremap R :%s//g<Left><Left>

" Window Management
" split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
nnoremap <M-s>h :set nosplitright<CR>:vsplit<CR>
nnoremap <M-s>j :set splitbelow<CR>:split<CR>
nnoremap <M-s>k :set nosplitbelow<CR>:split<CR>
nnoremap <M-s>l :set splitright<CR>:vsplit<CR>

" Place the two screens side by side (vertical)
nnoremap <M-s>m <C-w>t<C-w>H
" Place the two screens up and down (horizontal)
nnoremap <M-s>n <C-w>t<C-w>K

" Rotate screens
" nnoremap <M-s>rm <C-w>b<C-w>H
" nnoremap <M-s>rn <C-w>b<C-w>K

" Use <ALT> + new arrow keys for moving the cursor around windows
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

" Use shift arrow keys for moving the windows
" nnoremap <C-M-h> <C-w>H
" nnoremap <C-M-j> <C-w>J
" nnoremap <C-M-k> <C-w>K
" nnoremap <C-M-l> <C-w>L

" Tab Management
" Create a new tab
" nnoremap <M-n> :tabe<CR>
" nnoremap <M-q> :tabclose<CR>
" " nnoremap <M-n> :tabnew
" " switching tabs
" nnoremap <M-,> :-tabnext<CR>
" nnoremap <M-.> :+tabnext<CR>
" " Move the tabs
" nnoremap <M-<> :-tabmove<CR>
" nnoremap <M->> :+tabmove<CR>
" " Map alt-x keys to jump to a tab
" for i in range(1, 8)
"     exe "nnoremap <M-" . i . "> :tabnext " . i . "<CR>"
" endfor
" nnoremap <M-9> :tablast<CR>

" Buffer Management
nnoremap <M-q> :bd<CR>
" " switching buffer
nnoremap <M-,> :bp<CR>
nnoremap <M-.> :bn<CR>

" Clipboard
" set clipboard^=unnamed,unnnamedplus

" use the system clipboard
" might need to install a system clipboard tool such as : sudo pacman -S xclip / xsel
" let @+=@*<CR> for copying to both the clipboard and primary selection.
vnoremap Y "*y :let @+=@*<CR>
nnoremap <Leader>p "+p
nnoremap <Leader>P "+P

" RestoreRegister
" prevent replacing register on paste
function! RestoreRegister()
    let @" = s:restore_reg
    return ''
endfunction
function! s:Repl()
    let s:restore_reg = @"
    return "p@=RestoreRegister()\<CR>"
endfunction
vmap <silent> <expr> p <sid>Repl()

" Compile Function
" noremap <M-r> :call CompileRunGcc()<CR>
nnoremap <M-r> :call CompileRunGcc()<CR>
func! CompileRunGcc()
    exec "w"
    if &filetype == 'c'
        " exec "!g++ % -o %<"
        " exec "!gcc % -o %<"
        " exec "!time ./%<"
        :FloatermNew --autoclose=0 gcc % -o %< && time ./%<
    elseif &filetype == 'cpp'
        " set splitbelow
        " exec "!g++ -std=c++11 % -Wall -o %<"
        " :sp
        " :res -15
        " :term ./%<
        :FloatermNew --autoclose=0 g++ -std=c++11 % -Wall -o %< && ./%<
    elseif &filetype == 'java'
        " ==== compile & run ===
        " exec "!javac % && time java %<"
        " :FloatermNew --width=80 --height=40 javac % && time java %<
        :FloatermNew --autoclose=0 javac % && time java %<
        " === make & run ===
        " exec 'set makeprg=javac\ -g\ %'
        " exec "make"
        " exec "!time java %<"
        " === make ===
        " exec "make"
        " === for debug ===
        " exec "!time java -Xdebug -Xrunjdwp:server=y,transport=dt_socket,address=5005,suspend=y %<"
    elseif &filetype == 'rust'
        :FloatermNew --autoclose=0 rustc % && time ./%<
    elseif (&filetype == 'sh' || &filetype == 'zsh')
        " :!time bash %
        :FloatermNew --autoclose=0 time bash %
    elseif &filetype == 'python'
        " set splitbelow
        " :sp
        " :term python3 %
        :FloatermNew --autoclose=0 python3 %
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
        " :FloatermNew export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings .
        :FloatermNew --autoclose=0 export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings %
    elseif &filetype == 'go'
        " set splitbelow
        " :sp
        " :term go run .
        :FloatermNew --autoclose=0 time go run %
    elseif &filetype == 'nasm'
        exec "!nasm -f bin % -o %<.bin"
    elseif &filetype == 'lua'
        :FloatermNew --autoclose=0 time lua %
    endif
endfunc

" Openning Files
" Open the vimrc file anytime
nnoremap <Leader>fi :e ~/.config/nvim/init.lua<CR>

]]
