-- General config

HOME = os.getenv("HOME")
vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'
vim.g.nojoinspaces = true  -- " don't autoinsert two spaces after '.', '?', '!' for join command

-- basic settings
vim.o.encoding = "utf-8"
vim.o.backspace = "indent,eol,start" -- backspace works on every char in insert mode " set backspace=indent,eol,start; eol让退格键可以退到上一行
vim.o.completeopt = 'menuone,noinsert,noselect'
vim.o.history = 1000
vim.o.startofline = true
vim.o.textwidth = 80

-- Mapping waiting time
vim.o.timeout = false
vim.o.ttimeout = true
vim.o.ttimeoutlen = 100

-- Display
vim.o.showmatch  = true -- show matching brackets
vim.o.scrolloff = 3 -- always show 3 rows from edge of the screen
vim.o.synmaxcol = 300 -- stop syntax highlight after x lines for performance
vim.o.laststatus = 2 -- always show status line

vim.o.list = false -- do not display white characters
vim.o.foldenable = false
vim.o.foldlevel = 4 -- limit folding to 4 levels
vim.o.foldmethod = 'syntax' -- use language syntax to generate folds
vim.o.wrap = false --do not wrap lines even if very long
vim.o.eol = false -- show if there's no eol char
vim.o.showbreak= '↪' -- character to show when line is broken

-- Sidebar
vim.o.number = true -- line number on the left
vim.o.rnu = true -- line number on the left
vim.o.numberwidth = 2 -- always reserve 3 spaces for line number
vim.o.signcolumn = 'yes' -- keep 1 column for coc.vim  check
vim.o.modelines = 0
vim.o.showcmd = true -- display command in bottom bar

-- Search
vim.o.hlsearch  = true -- show matching brackets
vim.o.incsearch = true -- starts searching as soon as typing, without enter needed
vim.o.inccommand = 'split'
vim.o.ignorecase = true -- ignore letter case when searching
vim.o.smartcase = true -- case insentive unless capitals used in search

vim.o.matchtime = 2 -- delay before showing matching paren
vim.o.mps = vim.o.mps .. ",<:>"

-- White characters
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.tabstop = 2 -- 1 tab = 2 spaces
vim.o.softtabstop = 2 -- 1 tab = 2 spaces
vim.o.shiftwidth = 2 -- indentation rule
vim.o.formatoptions = 'qnj1' -- q  - comment formatting; n - numbered lists; j - remove comment when joining lines; 1 - don't break after one-letter word
vim.o.expandtab = true -- expand tab to spaces
vim.o.smarttab = true

-- Backup files
vim.cmd([[
silent !mkdir -p ~/.config/nvim/tmp/backup
silent !mkdir -p ~/.config/nvim/tmp/undo
silent !mkdir -p ~/.config/nvim/tmp/swap
silent !mkdir -p ~/.config/nvim/tmp/sessions
]])
vim.o.hidden = true -- use backup files
vim.o.backup = true -- use backup files
vim.o.undofile = true
vim.o.writebackup = false
vim.o.swapfile = false -- do not use swap file
vim.o.undodir = HOME .. '/.config/nvim/tmp/undo/'     -- undo files
vim.o.backupdir = HOME .. '/.config/nvim/tmp/backup/' -- backups
vim.o.directory = HOME .. '/.config/nvim/tmp/swap/'   -- swap files

-- " Save when losing focus
vim.cmd([[ au FocusLost * :silent! wall ]])

-- " Resize splits when the window is resized
vim.cmd([[ au VimResized * :wincmd = ]])

vim.cmd([[
  au FileType python                  set ts=4 sw=4
  au BufRead,BufNewFile *.md          set ft=mkd tw=80 syntax=markdown
  au BufRead,BufNewFile *.ppmd        set ft=mkd tw=80 syntax=markdown
  au BufRead,BufNewFile *.markdown    set ft=mkd tw=80 syntax=markdown
  au BufRead,BufNewFile *.slimbars    set syntax=slim
]])

-- Commands mode
vim.o.wildmenu = true -- on TAB, complete options for system command
vim.o.wildignore = 'deps,.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,.DS_Store,*.aux,*.out,*.toc,*/node_modules/**'

-- Only show cursorline in the current window and in normal mode.
vim.cmd([[
  augroup cline
      au!
      au WinLeave * set nocursorline
      au WinEnter * set cursorline
      au InsertEnter * set nocursorline
      au InsertLeave * set cursorline
  augroup END
]])

-- " syntax highlighting of search results
vim.cmd([[ au ColorScheme * highlight Search guibg=NONE guifg=Cyan gui=italic,underline,bold ]])

-- Stop comments on newline
vim.cmd([[ autocmd BufWinEnter * :set formatoptions-=c formatoptions-=r formatoptions-=o ]])

-- Disable lualine on Nvim-tree
-- vim.cmd([[ au BufEnter,BufWinEnter,WinEnter,CmdwinEnter * if !match(bufname('%'), 'NvimTree.*') | set laststatus=0 | else | set laststatus=2 | endif ]])

-- go to last location when opening a buffer  " make cursor remain the position of last quit
vim.cmd([[ au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif ]])

vim.o.background = 'dark'

vim.g.python3_host_prog = "/usr/bin/python3"
vim.g.python_host_prog = "/usr/bin/python2"

