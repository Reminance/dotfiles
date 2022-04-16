
vim.cmd[[
set nocompatible
let mapleader=" "
let localmapleader="\\"
set shiftwidth=2 tabstop=2 softtabstop=2 expandtab smarttab autoindent smartindent
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
set synmaxcol=200
set backspace=indent,eol,start " set backspace=indent,eol,start; eol让退格键可以退到上一行

set completeopt=menuone,noinsert,noselect
set signcolumn=yes
set updatetime=100
set encoding=UTF-8
" set clipboard+=unnamedplus " Copy paste between vim and everything else
set nojoinspaces " don't autoinsert two spaces after '.', '?', '!' for join command
" set showcmd " extra info at end of command line
" set wildignore+=*/node_modules/**
filetype plugin indent on

" change tabstop to 4 for python
au FileType python                  set ts=4 sw=4

"" Stop comments on newline
au BufWinEnter * :set formatoptions-=c formatoptions-=r formatoptions-=o

" syntax highlighting of search results
au ColorScheme * highlight Search guibg=NONE guifg=Cyan gui=italic,underline,bold
" highlight Comment cterm=italic gui=italic

" Make it obvious where 80 characters is
set textwidth=80
" 80字符竖线指示
" set colorcolumn=+1
" set colorcolumn=80
" highlight ColorColumn guibg=#181818

"" go to last location when opening a buffer  " make cursor remain the position of last quit
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Backups
set backup                        " enable backups
set noswapfile
silent !mkdir -p ~/.config/nvim/tmp/backup
silent !mkdir -p ~/.config/nvim/tmp/undo
silent !mkdir -p ~/.config/nvim/tmp/swap
silent !mkdir -p ~/.config/nvim/tmp/sessions
set backupdir=~/.config/nvim/tmp/backup,.
set directory=~/.config/nvim/tmp/swap,.
" use for u(undo) and Ctrl-r(redo)
if has('persistent_undo')
    set undofile
    set undodir=~/.config/nvim/tmp/undo,.
endif

" status line
" set laststatus=0  " disable status line
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
nnoremap <C-Space> :call ToggleHiddenStatusLine()<CR>

" for ctags
set tags=tags;/
" nnoremap <Leader>cg :!ctags --extra=+q --exclude=android-dto --languages=java -R .

" Cursor Movement
inoremap <C-b> <Left>
inoremap <C-f> <Right>
inoremap <C-a> <C-o>I
inoremap <C-e> <C-o>$
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
inoremap <C-g> <Esc>
map <C-g> <Esc>
inoremap <C-n> <Down>
inoremap <C-p> <Up>

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
nnoremap S <nop>
nnoremap R <nop>
nnoremap Q <nop>
nnoremap <C-q> :q<CR>
inoremap <C-q> <Esc>:q<CR>
nnoremap <C-M-q> :qa<CR>
inoremap <C-M-q> <Esc>:qa<CR>
inoremap <C-s> <Esc>:w<CR>
nnoremap <C-s> :w<CR>

" Source
nnoremap <Leader>s <nop>
nnoremap <Leader>si :source $MYVIMRC<CR>
nnoremap <Leader>s. :so %<CR>
vnoremap <Leader>sv y:execute @@<CR>:echo 'Sourced selection.'<CR>
nnoremap <Leader>sL ^vg_y:execute @@<CR>:echo 'Sourced line.'<CR>

" Save file as sudo on files that require root permission(by typing [Ctrl-s !]), note the '' symbol is type through Ctrl-v ctrl-s
cnoremap ! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" normal mode bindings
nnoremap <S-Down> :res +1<CR>
nnoremap <S-Up> :res -1<CR>
nnoremap <S-Left> :vertical resize-1<CR>
nnoremap <S-Right> :vertical resize+1<CR>

" Basic Mappings
nnoremap <Leader><Space> :nohlsearch<CR>

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
nnoremap <Leader>xe mzggg?G`z
" Rot13 encode {motion} text.
vnoremap <Leader>xe mzg?`z
" Select (charwise) the contents of the current line, excluding indentation.
" Great for pasting Python lines into REPLs.
nnoremap <Leader>vv ^vg_
" Perform dot commands over visual blocks:
vnoremap . :normal .<CR>
" Replace all is aliased to R.
nnoremap R :%s//g<Left><Left>

" Window Management
nnoremap <M-h> <Esc><C-w>h
nnoremap <M-j> <Esc><C-w>j
nnoremap <M-k> <Esc><C-w>k
nnoremap <M-l> <Esc><C-w>l
" split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
nnoremap <Leader>sh :set nosplitright<CR>:vsplit<CR>
nnoremap <Leader>sj :set splitbelow<CR>:split<CR>
nnoremap <Leader>sk :set nosplitbelow<CR>:split<CR>
nnoremap <Leader>sl :set splitright<CR>:vsplit<CR>

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
nnoremap <Leader>q :bd<CR>
" switching buffer
nnoremap <Leader><Left> :bp<CR>
nnoremap <Leader><Right> :bn<CR>

" terminal mode
tnoremap <Esc> <C-\><C-n>

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

let g:floaterm_width=0.8 " Default: 0.6
let g:floaterm_height=0.8 " Default: 0.6

" Compile Function
" noremap <M-r> :call CompileRunGcc()<CR>
noremap <Leader>R :call CompileRunGcc()<CR>
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

let g:python_host_prog='/usr/bin/python2'
let g:python3_host_prog='/usr/bin/python3'

" Openning Files
" Open the vimrc file anytime
nnoremap <Leader>fi :e ~/.config/nvim/init.lua<CR>
nnoremap <Leader>fg :e ~/.config/nvim/lua/general.lua<CR>
nnoremap <Leader>fp :e ~/.config/nvim/lua/plugins.lua<CR>
nnoremap <Leader>fs :e ~/.config/nvim/lua/plugin-settings.lua<CR>
nnoremap <Leader>fm :e ~/.config/nvim/lua/mappings.lua<CR>

]]
