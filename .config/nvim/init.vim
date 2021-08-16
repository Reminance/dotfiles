set nocompatible
set noswapfile
let mapleader=","
let maplocalleader="\\"
" set mouse=a " mouse support
set shiftwidth=4 tabstop=4 softtabstop=4 expandtab smarttab autoindent smartindent
set ignorecase smartcase incsearch showmatch hlsearch
set wrap formatoptions-=t "turn off Auto-wrap text using textwidth
set scrolloff=2
set nu rnu
set list
" set listchars=tab:▸\ ,trail:▫,eol:¬,extends:❯,precedes:❮,nbsp:␣,conceal:┊
" set listchars=tab:»\ ,trail:·,eol:↲,extends:❯,precedes:❮,nbsp:␣,conceal:┊
set listchars=tab:»\ ,trail:▫,eol:¬,extends:>,precedes:<,nbsp:␣,conceal:┊
set showbreak=↪
set inccommand=split
" set autochdir " auto change cwd
" Save when losing focus
au FocusLost * :silent! wall
" Resize splits when the window is resized
au VimResized * :wincmd =
" Don't try to highlight lines longer than 800 characters.
set synmaxcol=800
set backspace=indent,eol,start " set backspace=indent,eol,start; eol让退格键可以退到上一行

" for ctags
set tags=tags;/
nnoremap <Leader>cg :!ctags --extra=+q --languages=java -R .
" nnoremap <Leader>cg :!ctags --extra=+q --exclude=android-dto --languages=java -R .

" Cursor Movement
" insert mode bindings
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-b> <Left>
inoremap <C-f> <Right>
inoremap <M-f> <S-Right>
inoremap <M-b> <S-Left>
inoremap <C-g> <Esc>
map <C-g> <Esc>
" inoremap <C-k> <C-o>D
" inoremap <C-n> <Down>
" inoremap <C-p> <Up>

" command line mode bindings
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>

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
" syntax highlighting of search results
au ColorScheme * highlight Search guibg=NONE guifg=Cyan gui=italic,underline,bold

" Line Return
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif " make cursor remain the position of last quit

" Backups
set backup                        " enable backups
silent !mkdir -p ~/.config/nvim/tmp/backup
silent !mkdir -p ~/.config/nvim/tmp/undo
"silent !mkdir -p ~/.config/nvim/tmp/sessions
set backupdir=~/.config/nvim/tmp/backup,.
set directory=~/.config/nvim/tmp/backup,.
" use for u(undo) and Ctrl-r(redo)
if has('persistent_undo')
    set undofile
    set undodir=~/.config/nvim/tmp/undo,.
endif

" Save & quit
nnoremap s <nop>
nnoremap R <nop>
nnoremap Q <nop>
nnoremap <C-q> :q<CR>
inoremap <C-q> <Esc>:q<CR>
nnoremap <Leader>qq :q!<CR>
" quit all the other windows except for current  " (:h only)(<C-w>o)
nnoremap <Leader>qw :only<CR>
" quit all the other tabs except for current  " (:h tabonly)
nnoremap <Leader>qt :tabonly<CR>
nnoremap <C-M-q> :qa<CR>
inoremap <C-M-q> :qa<CR>
inoremap <C-s> <Esc>:w<CR>
nnoremap <C-s> :w<CR>

" Source
nnoremap <M-s> <nop>
nnoremap <M-s>i :source $MYVIMRC<CR>
nnoremap <M-s>. :so %<CR>
vnoremap <M-s>v y:execute @@<CR>:echo 'Sourced selection.'<CR>
nnoremap <M-s>l ^vg_y:execute @@<CR>:echo 'Sourced line.'<CR>

" Save file as sudo on files that require root permission
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" normal mode bindings
nnoremap <M-S-Down> :res +5<CR>
nnoremap <M-S-Up> :res -5<CR>
nnoremap <M-S-Left> :vertical resize-5<CR>
nnoremap <M-S-Right> :vertical resize+5<CR>

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
" Replace all is aliased to S.
nnoremap S :%s//g<Left><Left>

" Window Management
" split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
nnoremap sh :set nosplitright<CR>:vsplit<CR>
nnoremap sj :set splitbelow<CR>:split<CR>
nnoremap sk :set nosplitbelow<CR>:split<CR>
nnoremap sl :set splitright<CR>:vsplit<CR>

" Place the two screens side by side (vertical)
nnoremap sm <C-w>t<C-w>H
" Place the two screens up and down (horizontal)
nnoremap sn <C-w>t<C-w>K

" Rotate screens
nnoremap srm <C-w>b<C-w>H
nnoremap srn <C-w>b<C-w>K

" Use <ALT> + new arrow keys for moving the cursor around windows
nnoremap <M-h> <C-w>h
nnoremap <M-j> <C-w>j
nnoremap <M-k> <C-w>k
nnoremap <M-l> <C-w>l

" Use shift arrow keys for moving the windows
nnoremap <C-M-h> <C-w>H
nnoremap <C-M-j> <C-w>J
nnoremap <C-M-k> <C-w>K
nnoremap <C-M-l> <C-w>L

" Tab Management
" Create a new tab
nnoremap <M-n> :tabe<CR>
nnoremap <M-q> :tabclose<CR>
" nnoremap <M-n> :tabnew
" switching tabs
nnoremap <M-,> :-tabnext<CR>
nnoremap <M-.> :+tabnext<CR>
" Move the tabs
nnoremap <M-<> :-tabmove<CR>
nnoremap <M->> :+tabmove<CR>
" Map alt-x keys to jump to a tab
for i in range(1, 8)
    exe "nnoremap <M-" . i . "> :tabnext " . i . "<CR>"
endfor
nnoremap <M-9> :tablast<CR>

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
    elseif (&filetype == 'sh' || &filetype == 'zsh')
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

" Vim
augroup filetype_vim
    au!
    au FileType vim setl foldmethod=indent foldlevel=99
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
    au Filetype vim nnoremap <buffer> <M-f> $F.egf
augroup END

" Openning Files
" Open the vimrc file anytime
nnoremap <Leader><Leader>i :e ~/.config/nvim/init.vim<CR>
" Open the plugins.vim file anytime
nnoremap <Leader><Leader>p :e ~/.config/nvim/plugins.vim<CR>
" Open the scratchpad anytime
" nnoremap <Leader><Leader>s :FloatermNew $EDITOR ~/.config/nvim/scratchpad.vim<CR>

" Plugins Settings
source ~/.config/nvim/plugins.vim

" work tool
source ~/.config/nvim/worktool.vim

" Machine Specifisc Settings
" adjust machine specific stuff
let has_machine_specific_file=1
if empty(glob('~/.config/nvim/_machine_specific.vim'))
    let has_machine_specific_file=0
    silent! exe "!cp ~/.config/nvim/default_configs/_machine_specific_default.vim ~/.config/nvim/_machine_specific.vim"
endif
source ~/.config/nvim/_machine_specific.vim
