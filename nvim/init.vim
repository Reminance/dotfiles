" ----------------------------------- VIMRC -----------------------------------
" PreSetup
    set nocompatible
    filetype on
    filetype indent on
    filetype plugin on
    filetype plugin indent on

" Basic Options
    " let $BASH_ENV = "~/.bash_profile"
    " set shell=/bin/bash
    let mapleader=","
    let maplocalleader = "\\"
    set hidden " 允许在有未保存的修改时切换缓冲区，此时的修改由 vim 负责保存
    set encoding=utf-8
    set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1
    set fileformats=unix,dos,mac
    set modelines=0
    set noshowmode
    set noshowcmd
    set noruler
    set ttyfast "should make scrolling faster
    set lazyredraw "same as above
    let &t_SI="\<Esc>]50;CursorShape=1\x7"
    let &t_SR="\<Esc>]50;CursorShape=2\x7"
    let &t_EI="\<Esc>]50;CursorShape=0\x7"
    set laststatus=0
    set shiftwidth=4 tabstop=4 softtabstop=4 expandtab smarttab autoindent smartindent
    set wrap
    set textwidth=80
    set nu rnu
    set colorcolumn=+1
    set list
    " set listchars=tab:▸\ ,trail:▫,eol:¬,extends:❯,precedes:❮,nbsp:␣,conceal:┊
    " set listchars=tab:»\ ,trail:·,eol:↲,extends:❯,precedes:❮,nbsp:␣,conceal:┊
    set listchars=tab:»\ ,trail:▫,eol:¬,extends:>,precedes:<,nbsp:␣,conceal:┊
    set showbreak=↪
    set matchtime=5
    set autoread autowrite
    set shiftround
    set signcolumn=yes
    set inccommand=split
    " set linebreak " auto wrap long lines with line break
    " set autochdir " auto change cwd
    " Save when losing focus
    au FocusLost * :silent! wall
    " Resize splits when the window is resized
    au VimResized * :wincmd =
    " fillchars
    " set fillchars=diff:⣿,vert:│
    " Don't try to highlight lines longer than 800 characters.
    set synmaxcol=800
    " (:h backspace?), default "indent,eol,start"; eol让退格键可以退到上一行
    set backspace=indent,eol,start

    " Cursorline & ColorColumn
        set cursorline
        " Only show cursorline in the current window and in normal mode.
        augroup cursorline
            au!
            au WinLeave,InsertEnter * set nocursorline
            au WinEnter,InsertLeave * set cursorline
        augroup END
        augroup colorcolumn
            au!
            au ColorScheme * highlight ColorColumn term=reverse ctermbg=1 guifg=#f9f9ff guibg=#242729
            au ColorScheme * highlight Folded term=reverse ctermbg=Black guifg=#00d6d6 guibg=NONE
        augroup end

    " Gui
        syntax on
        set background=dark
        set termguicolors " enable true colors support
        let $NVIM_TUI_ENABLE_TRUE_COLOR=1
        if !has('gui_running')
            set t_Co=256
        endif
        set t_ut=

    " Timeout
        " Time out on key codes but not mappings.
        " Basically this makes terminal Vim work sanely.
        set timeout
        set timeoutlen=1000
        set ttimeout
        set ttimeoutlen=10
        " for better coc experience
        set updatetime=200

    " Better Completion
        " (:help 'complete')
        set complete=.,w,b,u,t
        set completeopt=menuone,preview,noinsert,noselect
        set shortmess+=c

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
            set undodir=~/.config/nvim/tmp/undo,.
        endif

    " Wildmenu Completion
        set wildmenu
        set wildmode=longest:full
        set wildignore+=.hg,.git,.svn                    " Version control
        set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
        set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
        set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
        set wildignore+=*.spl                            " compiled spelling word lists
        set wildignore+=*.sw?                            " Vim swap files
        set wildignore+=*.DS_Store                       " OSX bullshit
        set wildignore+=*.luac                           " Lua byte code
        set wildignore+=migrations                       " Django migrations
        set wildignore+=*.pyc                            " Python byte code
        set wildignore+=*.orig                           " Merge resolution files

        " Clojure/Leiningen
        set wildignore+=classes
        set wildignore+=lib

    " Automatically deletes all trailing whitespace and newlines at end of file on save.
        autocmd BufWritePre * %s/\s\+$//e
        autocmd BufWritePre * %s/\n\+\%$//e
        autocmd BufWritePre *.[ch] %s/\%$/\r/e

" Basic Mappings
    " Save & quit
    nnoremap s <nop>
    nnoremap R <nop>
    nnoremap Q <nop>
    nnoremap <C-q> :q<CR>
    inoremap <C-q> <Esc>:q<CR>
    nnoremap <Leader>qq :q!<CR>
    nnoremap <C-M-q> :qa<CR>
    inoremap <C-M-q> :qa<CR>
    inoremap <C-s> <Esc>:w<CR>
    nnoremap <C-s> :w<CR>

    " Source
    nnoremap <M-x> <nop>
    nnoremap <M-x>si :source $MYVIMRC<CR>
    nnoremap <M-x>s. :so %<CR>
    vnoremap <M-x>sv y:execute @@<CR>:echo 'Sourced selection.'<CR>
    nnoremap <M-x>sl ^vg_y:execute @@<CR>:echo 'Sourced line.'<CR>

    " show the vim mappings
    nnoremap <Leader>vmn :nmap
    nnoremap <Leader>vmi :imap
    nnoremap <Leader>vmv :vmap

    " Save file as sudo on files that require root permission
    cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

    " normal mode bindings
    nnoremap <C-Down> :res +5<CR>
    nnoremap <C-Up> :res -5<CR>
    nnoremap <C-Left> :vertical resize-5<CR>
    nnoremap <C-Right> :vertical resize+5<CR>

    " Create Blank Newlines and stay in Normal mode
    nnoremap <silent> zj moo<Esc>`o
    nnoremap <silent> zk moO<Esc>`o

    " Basic Mappings
    nnoremap <Leader><Space> :nohlsearch<CR>
    " quit all the other windows except for current  " (:h only)(<C-w>o)
    nnoremap <Leader>qw :only<CR>
    " quit all the other tabs except for current  " (:h tabonly)
    nnoremap <Leader>qt :tabonly<CR>

    " insert a <++>
    inoremap <M-i> <++>
    " jump to next <++> and replace it
    nnoremap <M-Space> <Esc>/<++><CR>:nohlsearch<CR>c4l
    inoremap <M-Space> <Esc>/<++><CR>:nohlsearch<CR>c4l

    " indent blocks and keep selected
    vnoremap < <gv
    vnoremap > >gv

    " duplicate words
    nnoremap <Leader>dw /\(\<\w\+\>\)\_s*\1<CR>

    " Open up lazygit
    nnoremap <C-\>g :tabe<CR>:tabmove<CR>:term lazygit<CR>:setl nonu<CR>:setl nornu<CR>a

    " Open a terminal in vim
    nnoremap <C-\><C-h> :set nosplitright<CR>:vnew<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\>h :set nosplitright<CR>:vnew<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\><C-j> :set splitbelow<CR>:new<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\>j :set splitbelow<CR>:new<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\><C-k> :set nosplitbelow<CR>:new<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\>k :set nosplitbelow<CR>:new<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\><C-l> :set splitright<CR>:vnew<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    nnoremap <C-\>l :set splitright<CR>:vnew<CR>:term<CR>:setl nonu<CR>:setl nornu<CR>a
    " tnoremap <Esc> <C-\><C-n><Esc><CR>

    " reading source into vim(:h read) or :r! cat ~/.bashrc
    nnoremap <M-S-r> :r

    " help shortcut(:helpgrep i_^n)
    nnoremap <Leader>hg :helpgrep
    nnoremap <Leader>hn :cnext<CR>
    nnoremap <Leader>hp :cprev<CR>

    " open the quickfix list
    nnoremap <Leader>co :copen<CR>

    " toggle spell check
    nnoremap <Leader>spt :setl spell!<CR>
    " fix the last spell bad to the cursor; eg: [shcool], pressing ,sf will fix it to [school]
    nnoremap <Leader>spf [sz=
    " fix and jump back
    " nnoremap <Leader>sf mm[s1z=`m

    " quickly paste in command line; using <C-r>; (:h i_CTRL-R)[Insert the contents of a register.]
    nnoremap <Leader>Cp :<C-r>"

    " vim tutor
    nnoremap <Leader>vt :Tutor<CR>

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

    " templates
    augroup templates
        au!
        au BufNewFile *.sh 0r ~/.config/nvim/templates/sh.tpl
        au BufNewFile *.c 0r ~/.config/nvim/templates/c.tpl
        au BufNewFile *.cpp 0r ~/.config/nvim/templates/cpp.tpl
        au BufNewFile *.java 0r ~/.config/nvim/templates/java.tpl
        au BufNewFile *.go 0r ~/.config/nvim/templates/go.tpl
        au BufNewFile *.py 0r ~/.config/nvim/templates/py.tpl
        au BufNewFile *.html 0r ~/.config/nvim/templates/html.tpl
    augroup END

" Searching And Movement
    set ignorecase
    set smartcase
    set incsearch
    set showmatch
    set hlsearch
    set gdefault

    set scrolloff=2
    " Don't move on *
    nnoremap <silent> * mm*`m

    " Cursor Movement
        " insert mode bindings(emacs like)
        inoremap <C-a> <C-o>I
        inoremap <C-e> <C-o>A
        inoremap <C-b> <Left>
        inoremap <C-f> <Right>
        inoremap <M-f> <S-Right>
        inoremap <M-b> <S-Left>
        inoremap <M-e> <Esc>ea
        inoremap <C-d> <Del>
        inoremap <C-k> <C-o>D
        inoremap <M-k> <Esc>ddk$
        nnoremap <S-Del> ddkA
        inoremap <S-Del> <Esc>ddkA
        inoremap <C-g> <Esc>
        map <C-g> <Esc>
        " inoremap <C-n> <Down>
        " inoremap <C-p> <Up>

    " command line mode bindings
        cnoremap <C-a> <Home>
        cnoremap <C-e> <End>
        cnoremap <C-f> <Right>
        cnoremap <C-b> <Left>
        cnoremap <C-d> <Del>
        cnoremap <M-f> <S-Right>
        cnoremap <M-b> <S-Left>

    " Visual Mode */# from Scrooloose
        function! s:VSetSearch()
        let temp = @@
        norm! gvy
        let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
        let @@ = temp
        endfunction

        vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
        vnoremap # :<C-u>call <SID>VSetSearch()<CR>??<CR><c-o>

    " List navigation
        " nnoremap <Left>  :cprev<cr>zvzz
        " nnoremap <Right> :cnext<cr>zvzz
        " nnoremap <Up>    :lprev<cr>zvzz
        " nnoremap <Down>  :lnext<cr>zvzz

    " syntax highlighting of search results
        " for gui
        " au ColorScheme * highlight Search guibg=guibg guifg=guifg gui=italic,underline,bold
        au ColorScheme * highlight Search guibg=guibg guifg=Cyan gui=italic,underline,bold
        " for term
        " au ColorScheme * highlight Search ctermbg=black ctermfg=yellow term=underline

" Tags & Ctags
    set tags=./.tags;,tags

    " use CTRL-T & :ta<CR> jump backward or forward
    nnoremap <Leader>tg :!ctags -R<CR>
    nnoremap <Leader>tl :tags<CR>
    nnoremap <Leader>ts :ptselect <C-r><C-w><CR>
    nnoremap <Leader>tj :ptjump <C-r><C-w><CR>
    nnoremap <Leader>tn :ptnext<CR>
    nnoremap <Leader>tp :ptprevious<CR>

" Window Management
    " split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
    nnoremap sh :set nosplitright<CR>:vsplit<CR>
    nnoremap sj :set splitbelow<CR>:split<CR>
    nnoremap sk :set nosplitbelow<CR>:split<CR>
    nnoremap sl :set splitright<CR>:vsplit<CR>
    nnoremap seh :set nosplitright<CR>:vsplit
    nnoremap sej :set splitbelow<CR>:split
    nnoremap sek :set nosplitbelow<CR>:split
    nnoremap sel :set splitright<CR>:vsplit

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
    " nnoremap <Leader>n :tabe<CR>
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

" Ultisnips
    function! s:edit_snippets(snippets_name)
        exe 'vsp ~/.config/nvim/Ultisnips/'.a:snippets_name
    endfunction
    command! -bang -nargs=* EditUtilSnips call fzf#run({
                \   'source': 'ls -1 ~/.config/nvim/Ultisnips',
                \   'down': 20,
                \   'sink': function('<sid>edit_snippets')
                \ })

" Folding
    set foldlevelstart=0
    set foldcolumn=0

    " Tab to toggle folds.
    nnoremap <CR> za
    vnoremap <CR> za

    " Make <z0> recursively open whatever fold we're in, even if it's partially open.
    nnoremap z0 zczO

    let g:foldmarkerlhs=split(&foldmarker, ",")[0]
    let g:nucolwidth = &fdc + &number * &numberwidth
    function! MyFoldText()
        let windowwidth = winwidth(0) - g:nucolwidth - 3
        let foldedlinecount = v:foldend - v:foldstart
        let linerange = ' [' . v:foldstart . '-' . v:foldend . ']'
        let line = getline(v:foldstart)
        let line = substitute(line, g:foldmarkerlhs, '', 'g')
        let filldashcount = &textwidth - len(line) - len(linerange)
        let line = line . repeat('-', filldashcount) . linerange
        let fillspacecount = windowwidth - len(line) - len(foldedlinecount)
        return line . repeat(' ', fillspacecount) . foldedlinecount . ' '
    endfunction
    set foldtext=MyFoldText()

" Vim
    augroup filetype_vim
        au!
        au FileType vim setl foldmethod=indent foldlevel=0
        au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
        au Filetype vim nnoremap <buffer> <M-f> $F.egf
    augroup END

" Environments (GUI/Console)
    if has('gui_running')
        " GUI Vim

        set guifont=Menlo\ Regular\ for\ Powerline:h12

        " Remove all the UI cruft
        " 'guioptions' 'go'	string	(default "egmrLT"   (MS-Windows))
        set guioptions-=T
        set guioptions-=l
        set guioptions-=L
        set guioptions-=r
        set guioptions-=R

        highlight SpellBad term=underline gui=undercurl guisp=Orange

        " Different cursors for different modes.
        set guicursor=n-c:block-Cursor-blinkon0
        set guicursor+=v:block-vCursor-blinkon0
        set guicursor+=i-ci:ver20-iCursor

        if has("gui_macvim")
            " Full screen means FULL screen
            set fuoptions=maxvert,maxhorz

            " Use the normal HIG movements, except for M-Up/Down
            let macvim_skip_cmd_opt_movement = 1
            " no   <D-Left>       <Home>
        else
            " Non-MacVim GUI, like Gvim
        end
    else
        " Console Vim
        " For me, this means iTerm2, possibly through tmux

        " Mouse support
        set mouse=a
    endif

" Basic Funcion
    source ~/.config/nvim/function.vim

" Plugins Settings(Included lsp)
    source ~/.config/nvim/plugins.vim

" Markdown sneppets
    source ~/.config/nvim/snippets/_md_snippets.vim

" Openning Files
    " Open the vimrc file anytime
    nnoremap <Leader><Leader>i :e ~/.config/nvim/init.vim<CR>
    " Open the function.vim file anytime
    nnoremap <Leader><Leader>f :e ~/.config/nvim/function.vim<CR>
    " Open the plugins.vim file anytime
    nnoremap <Leader><Leader>p :e ~/.config/nvim/plugins.vim<CR>
    " Open the _md_snippets.vim file anytime
    nnoremap <Leader><Leader>m :e ~/.config/nvim/snippets/_md_snippets.vim<CR>
    " Open the scratchpad anytime
    nnoremap <Leader><Leader>s :FloatermNew $EDITOR ~/.config/nvim/scratchpad.vim<CR>

" Machine Specifisc Settings
    " adjust machine specific stuff
    let has_machine_specific_file=1
    if empty(glob('~/.config/nvim/_machine_specific.vim'))
        let has_machine_specific_file=0
        silent! exe "!cp ~/.config/nvim/default_configs/_machine_specific_default.vim ~/.config/nvim/_machine_specific.vim"
    endif
    source ~/.config/nvim/_machine_specific.vim
