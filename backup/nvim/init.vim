set nocompatible
let mapleader=" "
set shiftwidth=4 tabstop=4 softtabstop=4 expandtab smarttab autoindent smartindent
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
nnoremap <M-s>L ^vg_y:execute @@<CR>:echo 'Sourced line.'<CR>

" Save file as sudo on files that require root permission(by typing [Ctrl-s !]), note the '' symbol is type through Ctrl-v ctrl-s
cnoremap ! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" normal mode bindings
nnoremap <Down> :res +1<CR>
nnoremap <Up> :res -1<CR>
nnoremap <Left> :vertical resize-1<CR>
nnoremap <Right> :vertical resize+1<CR>

" Basic Mappings
nnoremap <Leader>, :nohlsearch<CR>

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
nnoremap <Leader>fi :e ~/.config/nvim/init.vim<CR>

" work tool
" source ~/.config/nvim/worktool.vim

" Machine Specifisc Settings
" adjust machine specific stuff
let has_machine_specific_file=1
if empty(glob('~/.config/nvim/_machine_specific.vim'))
    let has_machine_specific_file=0
    silent! exe "!cp ~/.config/nvim/default_configs/_machine_specific_default.vim ~/.config/nvim/_machine_specific.vim"
endif
source ~/.config/nvim/_machine_specific.vim

" Vim-plug Auto Load
" Auto load for the first time
if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
    echo "Downloading junegunn/vim-plug to manage plugins..."
    silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
    silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
    autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
" PlugInstall PlugClean PlugUpdate
" Language Server Protocol
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'

" cmp For ultisnips users.
Plug 'quangnguyen30192/cmp-nvim-ultisnips'

Plug 'glepnir/lspsaga.nvim'
Plug 'folke/trouble.nvim'
Plug 'onsails/lspkind-nvim'

" File Management
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

Plug 'kyazdani42/nvim-tree.lua'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'windwp/nvim-spectre'
nnoremap <leader>S :lua require('spectre').open()<CR>

" https://github.com/nvim-treesitter/nvim-treesitter/issues/1111
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

Plug 'hoob3rt/lualine.nvim'
Plug 'kyazdani42/nvim-web-devicons'

" tpope plugins
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'

Plug 'lewis6991/gitsigns.nvim'
Plug 'vimwiki/vimwiki', { 'on': ['VimwikiIndex'] }
Plug 'norcalli/nvim-colorizer.lua', { 'branch': 'color-editor' }
Plug 'folke/which-key.nvim'
Plug 'justinmk/vim-sneak'

" Find & Replace
Plug 'brooth/far.vim', { 'on': ['F', 'Far', 'Fardo'] }

" Bookmarks
Plug 'MattesGroeger/vim-bookmarks'

" Plug 'vim-test/vim-test', { 'on': ['TestNearest', 'TestLast', 'TestFile', 'TestSuite', 'TestVisit'] }
" Plug 'tweekmonster/startuptime.vim'
Plug 'akinsho/nvim-bufferline.lua'

Plug 'windwp/nvim-autopairs'
Plug 'junegunn/goyo.vim'

" vim-peekaboo
" Peekaboo extends " and @ in normal mode and <CTRL-R> in insert mode
" so you can see the contents of the registers.
" Plug 'junegunn/vim-peekaboo'

" Other visual enhancement
" Plug 'nathanaelkane/vim-indent-guides'
Plug 'Yggdroot/indentLine'
" Plug 'itchyny/vim-cursorword'

" Tagbar
Plug 'majutsushi/tagbar', { 'on': 'TagbarOpenAutoClose' }

" Snippets
" Track the engine.
Plug 'SirVer/ultisnips'
" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'

" Markdown
Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
Plug 'dhruvasagar/vim-table-mode', { 'on': 'TableModeToggle' }

" Other visual enhancement
Plug 'mhinz/vim-startify'
" Plug 'glepnir/dashboard-nvim'        " Dashboard
Plug 'ryanoasis/vim-devicons'

" floaterm
Plug 'voldikss/vim-floaterm'

" vim-easy-align
Plug 'junegunn/vim-easy-align'

" Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'connorholyday/vim-snazzy'
" Plug 'morhetz/gruvbox'
" Plug 'sjl/badwolf'
" Plug 'kyoz/purify', { 'rtp': 'vim' }
" Plug 'srcery-colors/srcery-vim'
" Plug 'ayu-theme/ayu-vim'
" Plug 'NLKNguyen/papercolor-theme'
" Plug 'crusoexia/vim-monokai'
" Plug 'sainnhe/sonokai'
" Plug 'martinsione/darkplus.nvim'

call plug#end()

" if (has("termguicolors"))
"   set termguicolors " enable true colors support
" endif
if !$TERM_PROGRAM =~ "Apple_Terminal"
  set termguicolors
endif
set background=dark " light or dark
let g:SnazzyTransparent=1
colorscheme snazzy
" let g:dracula_colorterm = -1
" let g:dracula_italic = 1
" colorscheme dracula
" colorscheme gruvbox
" colorscheme badwolf
" colorscheme darkplus
" colorscheme purify
" colorscheme srcery
" let ayucolor="light"  " for light version of theme
" let ayucolor="mirage" " for mirage version of theme
" let ayucolor="dark"   " for dark version of theme
" colorscheme ayu
" colorscheme PaperColor
" colorscheme monokai
" colorscheme sonokai

" justinmk/vim-sneak {{{
let g:sneak#label = 1
map s <Plug>Sneak_s
map S <Plug>Sneak_S
" map f <Plug>Sneak_f
" map F <Plug>Sneak_F
" map t <Plug>Sneak_t
" map T <Plug>Sneak_T
" }}}

" kdheepak/lazygit.nvim {{{
" nnoremap <silent> <leader>lg :LazyGit<CR>
" }}}

" Plug 'windwp/nvim-autopairs' {{{
lua << EOF
require('nvim-autopairs').setup()
EOF
" }}}

" norcalli/nvim-colorizer.lua {{{
lua require'colorizer'.setup()
" }}}

" lewis6991/gitsigns.nvim {{{
lua << EOF
  require('gitsigns').setup({})
EOF
" }}}

" lspkind-nvim settings {{
lua << EOF
require('lspkind').init({
    -- DEPRECATED (use mode instead): enables text annotations
    --
    -- default: true
    -- with_text = true,

    -- defines how annotations are shown
    -- default: symbol
    -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
    mode = 'symbol_text',

    -- default symbol map
    -- can be either 'default' (requires nerd-fonts font) or
    -- 'codicons' for codicon preset (requires vscode-codicons font)
    --
    -- default: 'default'
    preset = 'codicons',

    -- override preset symbols
    --
    -- default: {}
    symbol_map = {
      Text = "",
      Method = "",
      Function = "",
      Constructor = "",
      Field = "ﰠ",
      Variable = "",
      Class = "ﴯ",
      Interface = "",
      Module = "",
      Property = "ﰠ",
      Unit = "塞",
      Value = "",
      Enum = "",
      Keyword = "",
      Snippet = "",
      Color = "",
      File = "",
      Reference = "",
      Folder = "",
      EnumMember = "",
      Constant = "",
      Struct = "פּ",
      Event = "",
      Operator = "",
      TypeParameter = ""
    },
})
EOF
" }}

" nvim-cmp settings {{{
lua <<EOF
  -- Setup nvim-cmp.
  local cmp = require'cmp'
  local lspkind = require('lspkind')
  lspkind.init()
  cmp.setup({
    formatting = {
      format = lspkind.cmp_format({
      menu = {
        buffer = "[buf]",
        nvim_lsp = "[lsp]",
        nvim_lua = "[api]",
        path = "[path]",
        luasnip = "[snip]",
        ultisnips = "[snip]",
        tn = "[TabNine]",
        },
      mode = 'symbol', -- show only symbol annotations
      maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)

      -- The function below will be called before any actual modifications from lspkind
      -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
      -- before = function (entry, vim_item)
      -- ...
      -- return vim_item
      -- end
    })
    },
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
      -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    mapping = {
      ['<C-b>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
      ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
      ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
      ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ['<C-e>'] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    },
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      -- { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it. 
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline('/', {
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })

  -- -- Setup lspconfig.
  -- local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
  -- -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
  -- require('lspconfig')['<YOUR_LSP_SERVER>'].setup {
  --   capabilities = capabilities
  -- }
EOF
" }}}

" neovim/nvim-lspconfig {{{
lua << EOF
-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
vim.api.nvim_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd','<cmd>lua require(\'telescope.builtin\').lsp_definitions()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gt', '<cmd>lua require(\'telescope.builtin\').lsp_type_definitions()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua require(\'telescope.builtin\').lsp_references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<A-CR>', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- local servers = { 'bashls', 'clangd', 'gopls', 'tsserver', 'pyright' }
-- for _, lsp in pairs(servers) do
--   require('lspconfig')[lsp].setup {
--     on_attach = on_attach,
--     flags = {
--       -- This will be the default in neovim 0.7+
--       debounce_text_changes = 150,
--     }
--   }
-- end

-- Add additional capabilities supported by nvim-cmp
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local lspconfig = require('lspconfig')

-- Enable some language servers with the additional completion capabilities offered by nvim-cmp
local servers = { 'bashls', 'clangd', 'gopls', 'tsserver', 'pyright', 'rust_analyzer' }
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    -- on_attach = my_custom_on_attach,
    capabilities = capabilities,
  }
end

EOF
" }}}

" nvim-treesitter {{{
lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ensure_installed = { "html", "lua", "vim" }, -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  -- ignore_install = { "javascript" }, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    -- disable = { "c", "rust" },  -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    -- additional_vim_regex_highlighting = false,
  },
}
EOF
" }}}

" Plug 'hoob3rt/lualine.nvim' {{{
" theme = 'dracula',
lua << EOF
require('plenary.reload').reload_module('lualine', true)
require('lualine').setup({
  options = {
    -- theme = 'gruvbox',
    theme = 'dracula',
    disabled_types = { 'NvimTree' }
    },
  sections = {
    lualine_a = {"mode"},
    lualine_b = {"branch", "diff"},
    lualine_c = {"filename"},
    lualine_x = {
      {"diagnostics", sources = {"nvim_lsp"}},
      "encoding",
      "fileformat",
      "filetype"
      },
    lualine_y = {"progress"},
    lualine_z = {"location"}
    }
})
EOF
set laststatus=0  " disable status line after lua line plugin
" }}}

" vimwiki/vimwiki {{{
nnoremap <leader>vw :VimwikiIndex<CR>
"}}}

" kyazdani42/nvim-tree.lua {{{
" [NvimTree] following options are now set in the setup (:help nvim-tree.setup): nvim_tree_auto_close | nvim_tree_lsp_diagnostics
lua <<EOF
 require'nvim-tree'.setup {
      disable_netrw       = true,
      hijack_netrw        = true,
      open_on_setup       = false,
      ignore_ft_on_setup  = {},
      update_to_buf_dir   = {
        enable = true,
        auto_open = true,
      },
      auto_close          = false,
      open_on_tab         = false,
      hijack_cursor       = false,
      update_cwd          = false,
      diagnostics         = {
        enable = false,
        icons = {
          hint = "",
          info = "",
          warning = "",
          error = "",
        }
      },
      update_focused_file = {
        enable      = false,
        update_cwd  = false,
        ignore_list = {}
      },
      system_open = {
        cmd  = nil,
        args = {}
      },
      view = {
        width = 30,
        height = 30,
        side = 'left',
        auto_resize = false,
        mappings = {
          custom_only = false,
          list = {}
        }
      }
    }
EOF

nnoremap <leader>tt :NvimTreeToggle<CR>
nnoremap <leader>tr :NvimTreeRefresh<CR>
nnoremap <leader>tf :NvimTreeFindFile<CR>
" NvimTreeOpen, NvimTreeClose, NvimTreeFocus, NvimTreeFindFileToggle, and NvimTreeResize are also available if you need them
" set termguicolors " this variable must be enabled for colors to be applied properly
" a list of groups can be found at `:help nvim_tree_highlight`
" highlight NvimTreeFolderIcon guibg=blue
"}}}

" Tagbar
" Tagbar might need sudo pacman -S ctags
nnoremap <silent> T :TagbarOpenAutoClose<CR>

" indentLine
let g:indentLine_char_list = ['|', '¦', '┆', '┊']

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

" Far.vim
nnoremap <Leader>F :F  %<left><left>


" nvim-telescope settings {{
" Find files using Telescope command-line sugar.
nnoremap <M-S-n> <cmd>Telescope find_files<cr>
nnoremap <M-S-f> <cmd>Telescope live_grep<cr>
nnoremap <M-S-e> <cmd>Telescope buffers<cr>
nnoremap <M-S-h> <cmd>Telescope help_tags<cr>

lua << EOF
require('telescope').setup{
  pickers = {
    find_files = {
      hidden = true -- show hidden files in find_file
    },
    live_grep = {
      additional_args = function(opts)
        return {"--hidden"}  -- show hidden files in live_grep
      end
    },
  },
  defaults = {
       -- prompt_prefix = " ",
       prompt_prefix = "🔍 ",
    }
}
EOF
" " }}

" " fzf.vim
" nnoremap <M-S-l> :Lines<CR>
" " ripgrep
" nnoremap <M-S-f> :Rg<CR>
" " The Silver Searcher
" nnoremap <M-S-a> :Ag<CR>
" nnoremap <M-S-g> :GFiles<CR>
" nnoremap <M-S-d> :GFiles?<CR>
" nnoremap <M-S-n> :Files<CR>
" nnoremap <M-S-e> :Buffers<CR>
" nnoremap <M-S-h> :History<CR>
" nnoremap <M-S-t> :BTags<CR>
" nnoremap <M-S-c> :BCommits<CR>
" let g:fzf_preview_window='right:60%'
" let g:fzf_commits_log_options='--graph --color=always --format="%C(auto)%h%d %s %C(blue)%C(bold)%cr"'
" " nnoremap <M-S-c> :Commits<CR>

" rainbow
" let g:rainbow_active=1

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
            \ ]

" Open Startify
nnoremap <Leader>\\ :Startify<CR>
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
nnoremap <Leader>gs :G<CR>

" fugitive mergetool command
nnoremap <expr> <C-h> &diff ? ':diffget //2<CR>' : '<C-h>'
nnoremap <expr> <C-l> &diff ? ':diffget //3<CR>' : '<C-l>'
nnoremap <expr> <C-n> &diff ? ']c' : '<C-n>'
nnoremap <expr> <C-p> &diff ? '[c' : '<C-p>'
nnoremap <expr> <C-M-q> &diff ? ':call fugitive#DiffClose()<CR>' : ':qa<CR>'

" " normal vimdiff command
" if &diff
"     normal vimdiff command
"     nnoremap <C-h> :diffget LOCAL<CR>
"     nnoremap <C-Space> :diffget BASE<CR>
"     nnoremap <C-l> :diffget REMOTE<CR>
"     nnoremap <C-n> ]c
"     nnoremap <C-p> [c
" endif

" liuchengxu/vim-which-key
" let g:mapleader="\<Space>"
" nnoremap <silent> <Leader>      :<C-u>WhichKey '<Space>'<CR>
" let g:mapleader=','
" let g:maplocalleader='\'
" nnoremap <silent> <Leader>      :<C-u>WhichKey ','<CR>
" nnoremap <silent> <LocalLeader> :<C-u>WhichKey  ','<CR>

" folke/which-key.nvim
lua require("which-key").setup {}

" floaterm
" let g:floaterm_keymap_toggle='<Leader>/'
" let g:floaterm_keymap_next='<Leader>f.'
" let g:floaterm_keymap_prev='<Leader>f,'
" let g:floaterm_keymap_new='<Leader>f+'
" let g:floaterm_keymap_hide='<Leader>fh'
" let g:floaterm_keymap_show='<Leader>fs'
" let g:floaterm_keymap_kill='<Leader>fq'
" let g:floaterm_opener='edit'
" let g:floaterm_autoinsert=1
" let g:floaterm_width=0.6
" let g:floaterm_height=0.6
" " let g:floaterm_autoclose=1
" nnoremap <silent> <Leader>fP :FloatermNew --wintype=popup --height=6<CR>
" nnoremap <silent> <Leader>ff :FloatermNew fzf<CR>
" nnoremap <silent> <Leader>fg :FloatermNew lazygit<CR>
" nnoremap <silent> <Leader>fn :FloatermNew node<CR>
" nnoremap <silent> <Leader>fp :FloatermNew python<CR>
" nnoremap <silent> <Leader>fh :FloatermNew htop<CR>
" nnoremap <silent> <Leader>fd :FloatermNew ncdu<CR>
" tnoremap <silent> <C-q> <C-\><C-n>:FloatermKill<CR>

" vim-easy-align
" Start interactive EasyAlign in visual mode (e.g. vipga); custom:vip<Leader>ea
xmap <Leader>ea <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip); custom:<Leader>eaip
nmap <Leader>ea <Plug>(EasyAlign)

" Goyo plugin makes text more readable when writing prose:
map <leader>G :Goyo \| set bg=dark \| set linebreak<CR>

" 'akinsho/nvim-bufferline.lua' {{{
lua require('bufferline').setup{}
" nnoremap <silent> gb :BufferLinePick<CR>
" }}}

