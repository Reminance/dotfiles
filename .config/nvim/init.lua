-- [[ Setting options ]]
-- See `:help vim.o`

-- finding keybindings setup location
-- :verbose nmap <C-j>

-- Set highlight on search
vim.o.hlsearch = true

-- Make line numbers default
vim.opt.nu = true
vim.opt.relativenumber = true
vim.opt.tabstop = 2
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smartindent = true

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.config/nvim/tmp/undo"
vim.opt.undofile = true

vim.opt.incsearch = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- vim substitute command -- vim 正则匹配替换与引用
-- :%s/\(\w\+\)/select '\1' tableName, id, create_time from \1 order by id desc limit 1/g
-- select 'table_name' tableName, id, create_time from table_name order by id desc limit 1
-- 在上述命令中，:%s 表示将整个文件中的匹配项替换为指定文本。\(\w\+\) 表示用于匹配每个表名的正则表达式，其中 \w\+ 表示匹配任何连续的字母、数字或下划线。\1 表示反向引用前面的括号，即所匹配的表名。而 select '\1' tableName, id, create_time from \1 order by id desc limit 1 则是用于替换的 SQL 语句。/g 则表示替换所有匹配项。
-- :%s/\w\+/'&'/g
-- apple, banana, cherry, date, elderberry
-- 'apple', 'banana', 'cherry', 'date', 'elderberry'
-- 在上述命令中，% 表示将整个文件加载到 vim 缓冲区域中进行操作 s 表示代表 substitute - 替代。 \w\+ 匹配一个或多个单词字符，即匹配任何连续的字母、数字或下划线。'&' 代表当前匹配的文本本身，即标记要被占位符拼接的文本。g 则表示要替换所有匹配项而不仅仅是第一个。
--
-- plantuml entiry --> Table
-- :%s/entity ""\<\(\w\+\)\>"/Table("\1"")/g

-- disable statusline
vim.opt.laststatus = 0

-- Finding files - Search down into subfolders
vim.opt.path:append { '**' }

-- Use P to paste without yanking the deleted text. :help v_P
vim.keymap.set('v', 'p', 'P')

-- inccommand, The command formerly known as live substitution
-- vim.opt.inccommand = 'split'

-- -- These are to cancel the default behavior of d, D, c, C, x, X to put the text they delete in the default register.
-- -- Note that this means e.g. "ad won't copy the text into register a anymore. You have to explicitly yank it.
-- vim.keymap.set('n', 'd', '"_d')
-- vim.keymap.set('v', 'd', '"_d')
-- vim.keymap.set('n', 'D', '"_D')
-- vim.keymap.set('v', 'D', '"_D')
-- vim.keymap.set('n', 'c', '"_c')
-- vim.keymap.set('v', 'c', '"_c')
-- vim.keymap.set('n', 'C', '"_C')
-- vim.keymap.set('v', 'C', '"_C')
-- vim.keymap.set('n', 'x', '"_x')
-- vim.keymap.set('v', 'x', '"_x')

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 500
vim.wo.signcolumn = 'yes'

-- Set colorscheme
vim.o.termguicolors = true
-- for transparency
-- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
-- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- [[ Basic Keymaps ]]
-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Vim: quit if buffer list is empty; https://superuser.com/questions/668528/vim-quit-if-buffer-list-is-empty
vim.cmd[[
func SmartQuit()
  if expand('%') == '' && ( len( filter( range(1, bufnr('$')),  'buflisted(v:val)' ) )  == 1 )
    exe 'quit!'
  else
    exe 'bdel!'
  endif
endfunc
nnoremap <silent> <M-q> :call SmartQuit()<CR>
vnoremap <silent> <M-q> :call SmartQuit()<CR>
inoremap <silent> <M-q> <Esc>:call SmartQuit()<CR>
]]

-- keymaps
vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", "R", "<nop>")
vim.keymap.set("n", "<C-M-q>", ":qa<CR>")
vim.keymap.set("i", "<C-M-q>", "<ESC>:qa<CR>")
vim.keymap.set("n", "<C-q>", ":q<CR>")
vim.keymap.set("i", "<C-q>", "<ESC>:q<CR>")
vim.keymap.set("v", "<C-q>", "<ESC>:q<CR>")
vim.keymap.set("n", "<C-s>", ":w<CR>")
vim.keymap.set("i", "<C-s>", "<ESC>:w<CR>")
vim.keymap.set("i", "<C-n>", "<Down>")
vim.keymap.set("i", "<C-p>", "<Up>")
vim.keymap.set("i", "<C-k>", "<C-o>D")
vim.keymap.set({ 'i', 'c' }, "<C-b>", "<Left>")
vim.keymap.set({ 'i', 'c' }, "<C-f>", "<Right>")
vim.keymap.set({ 'i', 'c' }, "<C-a>", "<Home>")
vim.keymap.set({ 'i', 'c' }, "<C-e>", "<End>")
vim.keymap.set({ 'i', 'c' }, "<M-b>", "<S-Left>")
vim.keymap.set({ 'i', 'c' }, "<M-f>", "<S-Right>")
-- -- Allow saving of files as sudo when I forgot to start vim using sudo.
-- vim.keymap.set('c', "w!!", "w !sudo tee > /dev/null %") -- use https://github.com/lambdalisue/suda.vim instead

-- move selected lines up/down and keep selected
vim.keymap.set("v", "J", ":move '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":move '<-2<CR>gv=gv")
-- indent blocks and keep selected
vim.keymap.set("v", "H", "<gv")
vim.keymap.set("v", "L", ">gv")

-- Replace all is aliased to R.
vim.keymap.set("n", "R", ":%s//g<Left><Left>")

-- count the number of occurrences of a word
vim.keymap.set("n", "<Leader>o", ":%s///ng<Left><Left><Left><Left>")

-- sort and uniq current buffer
vim.keymap.set("n", "<Leader>su", ":sort u<CR>")
vim.keymap.set("v", "<Leader>su", ":'<,'>sort u<CR>")

-- -- Window Management
vim.keymap.set("n", "<M-h>", "<Esc><C-w>h")
vim.keymap.set("n", "<M-j>", "<Esc><C-w>j")
vim.keymap.set("n", "<M-k>", "<Esc><C-w>k")
vim.keymap.set("n", "<M-l>", "<Esc><C-w>l")

-- split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
vim.keymap.set("n", "<Leader>sh", ":set nosplitright<CR>:vsplit<CR>")
vim.keymap.set("n", "<Leader>sj", ":set splitbelow<CR>:split<CR>")
vim.keymap.set("n", "<Leader>sk", ":set nosplitbelow<CR>:split<CR>")
vim.keymap.set("n", "<Leader>sl", ":set splitright<CR>:vsplit<CR>")

-- normal mode bindings for resizing windows, <Option-arrow> for Mac, <C-M-hjkl> for Linux
vim.keymap.set("n", "<C-M-h>", ":vertical resize-1<CR>")
vim.keymap.set("n", "<C-M-j>", ":res +1<CR>")
vim.keymap.set("n", "<C-M-k>", ":res -1<CR>")
vim.keymap.set("n", "<C-M-l>", ":vertical resize+1<CR>")

-- -- Tab Management
-- vim.cmd [[
-- nnoremap <M-n> :tabnew<CR>
-- nnoremap <M-q> :tabclose<CR>
-- " switching tabs
-- nnoremap <M-,> :-tabnext<CR>
-- nnoremap <M-.> :+tabnext<CR>
-- " Move the tabs
-- nnoremap <M-<> :-tabmove<CR>
-- nnoremap <M->> :+tabmove<CR>
-- " " Map alt-x keys to jump to a tab
-- " for i in range(1, 8)
-- "     exe "nnoremap <M-" . i . "> :tabnext " . i . "<CR>"
-- " endfor
-- " nnoremap <M-9> :tablast<CR>
-- ]]

-- yank to system clipboard
vim.keymap.set("v", "Y", [["*y :let @+=@*<CR>]])

-- Rot13 encode {motion} text.
vim.keymap.set("v", "<Leader>xe", "mzg?`z")

-- base64 encode/decode
vim.cmd[[
vnoremap <leader>b y:let @"=system('base64', @")<cr>gvP
vnoremap <leader>B y:let @"=system('base64 --decode', @")<cr>gvP
]]

-- -- Remap for dealing with word wrap
-- vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
-- vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- go to last location when opening a buffer  " make cursor remain the position of last quit
vim.cmd [[au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif]]

-- syntax highlighting of search results
vim.cmd [[au ColorScheme * highlight Search guibg=NONE guifg=Cyan gui=italic,underline,bold]]

-- Don't move on *
vim.keymap.set("n", "*", "mm*`m", { noremap = true, silent = true })
-- Visual Mode * from Scrooloose
vim.cmd [[
function! s:VSetSearch()
    let temp = @@
    norm! gvy
    let @/ = '\V' . substitute(escape(@@, '\'), '\n', '\\n', 'g')
    let @@ = temp
endfunction
vnoremap * :<C-u>call <SID>VSetSearch()<CR>//<CR><c-o>
]]

-- nohlsearch
vim.keymap.set("n", "<Leader><Space>", ":nohlsearch<CR>", { noremap = true, silent = true })

-- Openning Files
-- Open the vimrc file anytime
vim.keymap.set("n", "<Leader>fi", ":e ~/.config/nvim/init.lua<CR>", { noremap = true, silent = true })
-- Open the notes file anytime
vim.keymap.set("n", "<Leader>fn", ":e ~/workspace/work-tools/notes/notes.md<CR>", { noremap = true, silent = true })
-- source $MYVIMRC
vim.keymap.set("n", "<Leader>si", ":source $MYVIMRC<CR>", { noremap = true, silent = true })
-- reload current file
vim.keymap.set("n", "<Leader>ff", ":e<CR>", { noremap = true, silent = true })

vim.cmd [[
noremap <Leader>R :call CompileRunGcc()<CR>
func! CompileRunGcc()
    exec "w"
    if &filetype == 'c'
        " exec "!g++ % -o %<"
        " exec "!gcc % -o %<"
        " exec "!time ./%<"
        " :FloatermNew --autoclose=0 gcc % -o %< && time ./%<
        :TermExec cmd="gcc % -o %< && time ./%<"
    elseif &filetype == 'cpp'
        " set splitbelow
        " exec "!g++ -std=c++11 % -Wall -o %<"
        " :sp
        " :res -15
        " :term ./%<
        " :FloatermNew --autoclose=0 g++ -std=c++11 % -Wall -o %< && ./%<
        :TermExec cmd="g++ -std=c++11 % -Wall -o %< && ./%<"
    elseif &filetype == 'java'
        " ==== compile & run ===
        " exec "!javac % && time java %<"
        " :FloatermNew --width=80 --height=40 javac % && time java %<
        " :FloatermNew --autoclose=0 javac % && time java %<
        :TermExec cmd="javac % && time java %<"
        " === make & run ===
        " exec 'set makeprg=javac\ -g\ %'
        " exec "make"
        " exec "!time java %<"
        " === make ===
        " exec "make"
        " === for debug ===
        " exec "!time java -Xdebug -Xrunjdwp:server=y,transport=dt_socket,address=5005,suspend=y %<"
    elseif &filetype == 'rust'
        " :FloatermNew --autoclose=0 rustc % && time ./%<
        :TermExec cmd="rustc % && time ./%<"
    elseif (&filetype == 'sh' || &filetype == 'zsh')
        " :!time bash %
        " :FloatermNew --autoclose=0 time bash %
        :TermExec cmd="time bash %"
    elseif &filetype == 'python'
        " set splitbelow
        " :sp
        " :term python3 %
        " :FloatermNew --autoclose=0 python3 %
        :TermExec cmd="python3 %"
    elseif &filetype == 'javascript'
        " set splitbelow
        " :sp
        " :term export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings .
        " :FloatermNew export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings .
        " :FloatermNew --autoclose=0 export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings %
        :TermExec cmd="export DEBUG="INFO,ERROR,WARNING"; node --trace-warnings %"
    elseif &filetype == 'go'
        " set splitbelow
        " :sp
        " :term go run .
        " :FloatermNew --autoclose=0 time go run %
        :TermExec cmd="time go run %"
    elseif &filetype == 'nasm'
        " exec "!nasm -f bin % -o %<.bin"
        :TermExec cmd="nasm -f bin % -o %<.bin"
    elseif &filetype == 'lua'
        " :FloatermNew --autoclose=0 time lua %
        :TermExec cmd="time lua %"
    elseif &filetype == 'typescript'
        :TermExec cmd="time tsc % && node %<.js"
    " elseif &filetype == 'html'
    "     silent! exec "!".g:mkdp_browser." % &"
    " elseif &filetype == 'vimwiki'
    "     exec "MarkdownPreview"
    " elseif &filetype == 'markdown'
    "     exec "MarkdownPreview"
    " elseif &filetype == 'tex'
    "     " silent! exec "VimtexStop"
    "     " silent! exec "VimtexCompile"
    "     silent! exec "LLPStartPreview"
    " elseif &filetype == 'dart'
    "     CocCommand flutter.run -d iPhone\ 11\ Pro
    "     CocCommand flutter.dev.openDevLog
    endif
endfunc
]]

-- enable markdown_folding, using Neovim's runtime filetype
vim.cmd('let g:markdown_folding = 1') -- This setting seems to enable folding from Neovim's runtime filetype, https://github.com/neovim/neovim/blob/master/runtime/ftplugin/markdown.vim

-- shortcut to TransformTodoStatus symbol
vim.cmd [[
noremap mt :call TransformTodoStatus('✅','🏷️')<CR>
noremap md :call TransformTodoStatus('🏷️','✅')<CR>
function! TransformTodoStatus(from, to)
  let line=getline('.')
  " try to match symbol with line end trailing space
  if line =~ "^\.*" . a:from . "\\s*$"
    norm mm
    exe '.s/' . a:from . '\s*$/' . a:to . '/g'
    norm `m
  else
    if line =~ "^\.*" . a:to . "\\s*$"
      echo "status is already do nothing"
    else
      norm mm
      exe ':normal! A ' . a:to
      norm `m
    endif
  endif
endfunction
]]

-- add additional keybinding to expand luasnip snippet
vim.cmd [[imap <silent><expr> <C-\> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<C-\>']]

-- shortcut for toggle case, base on arthurxavierx/vim-caser, don't need title case, sentence case for now...
local currentCase = ""
function ToggleCase()
  if currentCase == "" then
    currentCase = "snake_case"
    vim.fn['caser#DoAction']("SnakeCase", vim.fn.visualmode())
  elseif currentCase == "snake_case" then
    currentCase = "camelCase"
    vim.fn['caser#DoAction']("CamelCase", vim.fn.visualmode())
  elseif currentCase == "camelCase" then
    currentCase = "MixedCase"
    vim.fn['caser#DoAction']("MixedCase", vim.fn.visualmode())
  elseif currentCase == "MixedCase" then
    currentCase = "dash-case"
    vim.fn['caser#DoAction']("KebabCase", vim.fn.visualmode())
  elseif currentCase == "dash-case" then
    currentCase = "UPPER_CASE"
    vim.fn['caser#DoAction']("UpperCase", vim.fn.visualmode())
  elseif currentCase == "UPPER_CASE" then
    currentCase = "dot.case"
    vim.fn['caser#DoAction']("DotCase", vim.fn.visualmode())
  elseif currentCase == "dot.case" then
    currentCase = "space case"
    vim.fn['caser#DoAction']("SpaceCase", vim.fn.visualmode())
  elseif currentCase == "space case" then
    currentCase = "snake_case"
    vim.fn['caser#DoAction']("SnakeCase", vim.fn.visualmode())
  --   currentCase = "Title Case"
  --   vim.fn['caser#DoAction']("TitleCase", vim.fn.visualmode())
  -- elseif currentCase == "Title Case" then
  --   currentCase = "Sentence case"
  --   vim.fn['caser#DoAction']("SentenceCase", vim.fn.visualmode())
  -- elseif currentCase == "Sentence case" then
  end
  vim.api.nvim_feedkeys('gv', 'n', false)
end
vim.api.nvim_set_keymap('v', '<M-U>', ':lua ToggleCase()<CR>', { noremap = true, silent = true })

-- for terryma/vim-expand-region
vim.cmd[[
map <C-=> <Plug>(expand_region_expand)
map <C-_> <Plug>(expand_region_shrink)
]]

-- suda.vim
vim.cmd('let g:suda_smart_edit = 1')

-- for liuchengxu/vista.vim
vim.keymap.set("n", "\\", ":Vista!!<CR>", { noremap = true, silent = true })

local options = { noremap = true, silent = true }

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
  'numToStr/Comment.nvim', -- "gc" to comment visual regions/lines
  'junegunn/vim-easy-align',
  'tpope/vim-surround',
  -- gsm/gsp MixedCase, gsc camelCase, gs_ snake_case, gsu/gsU UPPER_CASE, gst Title Case, gss Sentence case, gs<space> space case, gs-/gsk dash-case or kebab-case, gsK Title-Dash-Case or Title-Kebab-Case, gs. dot.case
  'arthurxavierx/vim-caser', -- https://github.com/arthurxavierx/vim-caser
  'terryma/vim-expand-region',
  'lambdalisue/suda.vim',
  'liuchengxu/vista.vim',
  -- Fuzzy Finder (files, lsp, etc)
  { 'nvim-telescope/telescope.nvim',   branch = '0.1.x',    dependencies = { 'nvim-lua/plenary.nvim' } },
  -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
  -- { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', cond = vim.fn.executable 'make' == 1 }
  -- colorscheme
  'connorholyday/vim-snazzy',
  'dracula/vim',
  { "catppuccin/nvim",                 name = "catppuccin", priority = 1000 },
  { "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
  'mbbill/undotree',
  'tpope/vim-fugitive',
  'lewis6991/gitsigns.nvim',
  {'akinsho/bufferline.nvim', version = "*", dependencies = 'nvim-tree/nvim-web-devicons'},
  'nvim-lualine/lualine.nvim', -- Fancier statusline
  'mhinz/vim-startify',
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    }
  },
  -- icons
  'ryanoasis/vim-devicons',
  {
    "nvim-tree/nvim-tree.lua",
    version = "*",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("nvim-tree").setup {}
    end,
  },
  'windwp/nvim-autopairs',
  'norcalli/nvim-colorizer.lua',
  -- 'voldikss/vim-floaterm',
  { "akinsho/toggleterm.nvim", version = '*',                         opts = { --[[ things you want to change go here]] } },
  'brooth/far.vim',
  -- file navigation
  -- {"junegunn/fzf.vim"},
  -- -- Draw ASCII diagrams in Neovim.
  -- diffview
  { 'sindrets/diffview.nvim',  dependencies = 'nvim-lua/plenary.nvim' },
  { "folke/neodev.nvim",       opts = {} },
  -- LSP Support
  { 'neovim/nvim-lspconfig' }, -- Required
  {
    "williamboman/mason.nvim",
    build = ":MasonUpdate"                 -- :MasonUpdate updates registry contents
  },
  { 'williamboman/mason-lspconfig.nvim' }, -- Optional
  -- Autocompletion
  { 'hrsh7th/nvim-cmp' },
  { 'hrsh7th/cmp-buffer' },
  { 'hrsh7th/cmp-path' },
  { 'hrsh7th/cmp-cmdline' },
  { 'saadparwaiz1/cmp_luasnip' },
  { 'hrsh7th/cmp-nvim-lsp' },
  -- {'hrsh7th/cmp-copilot'}, --produce lattency issue, makes completion laggy an slow -- :Copilot setup
  -- {'hrsh7th/cmp-nvim-lua'},
  -- Snippets
  { 'L3MON4D3/LuaSnip' },
  { 'rafamadriz/friendly-snippets' },
  -- lsp_signature
  { 'ray-x/lsp_signature.nvim' },
  -- {
  --   "rcarriga/nvim-dap-ui",
  --   dependencies = {
  --     { "mfussenegger/nvim-dap" },
  --     { "mfussenegger/nvim-dap-python" },
  --     { "leoluz/nvim-dap-go" },
  --     { "nvim-neotest/nvim-nio" },
  --   }
  -- },
  -- { 'rcarriga/cmp-dap' },
  -- 'mfussenegger/nvim-jdtls',
  {'github/copilot.vim'},
  -- -- markdown-preview
  -- { "iamcco/markdown-preview.nvim", run = "cd app && npm install", setup = function() vim.g.mkdp_filetypes = { "markdown" } end, ft = { "markdown" }, },
  -- {
  --   'kristijanhusak/vim-dadbod-ui',
  --   dependencies = {
  --     { 'tpope/vim-dadbod', lazy = true },
  --     { 'kristijanhusak/vim-dadbod-completion', ft = { 'sql', 'mysql', 'plsql' }, lazy = true },
  --   },
  --   cmd = {
  --     'DBUI',
  --     'DBUIToggle',
  --     'DBUIAddConnection',
  --     'DBUIFindBuffer',
  --   },
  --   init = function()
  --     -- Your DBUI configuration
  --     vim.g.db_ui_use_nerd_fonts = 1
  --   end,
  -- },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
    },
  },
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    opts = {
      -- add any options here
    },
    dependencies = {
      -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
      "MunifTanjim/nui.nvim",
      -- OPTIONAL:
      --   `nvim-notify` is only needed, if you want to use the notification view.
      --   If not available, we use `mini` as the fallback
      "rcarriga/nvim-notify",
    }
  },
}

-- LS0gXCB7ICduYW1lJzogJ3dzc19mbG93X3Rlc3QnLCAndXJsJzogJ215c3FsOi8vYXdzX3Rlc3QtdF93c3NfZmxvd190ZXN0LWx0X3dzc19mbG93X3Rlc3Q6ZGVNMSNAUm4yU0lFcEl5QDEwLjEyMy40LjQ5OjI4ODMvd3NzX2Zsb3dfdGVzdCcgfSwKdmltLmNtZFtbCmxldCBnOmRicyA9IFsKXCB7ICduYW1lJzogJ2xvY2FsaG9zdCcsICd1cmwnOiAnbXlzcWw6Ly8vJyB9LApcIHsgJ25hbWUnOiAnd21zX2RldicsICd1cmwnOiAnbXlzcWw6Ly9sdF93bXNfZGV2X3J3X3RlOmg4MFRMQTB1RmZYaEoxZlNAb3BzLXdtcy1kYi1kZXYwMS5zaGVpbi5jb206MzMxMCcgfSwKXCB7ICduYW1lJzogJ3d3c19ldXJfZGV2JywgJ3VybCc6ICdteXNxbDovL2x0X3d3c19ldXJfZGV2X3J3Ond3c2h1aGF3Zjc4NjI4QDEwLjEyMi4zLjExOjQ0MDYnIH0sClwgeyAnbmFtZSc6ICd3bWRfZXVyX2RldicsICd1cmwnOiAnbXlzcWw6Ly9sdF93bWRfZXVyX3Rlc3Rfcnc6d21kZHVoYXdmNzg5MmNAMTAuMTIyLjMuMTE6NDQwNicgfSwKXCB7ICduYW1lJzogJ3dtc19tb25nb2RiX2RldicsICd1cmwnOiAnbW9uZ29kYjovL2x0X3N1cHBseV93bXM6Zlltc05ROFcxMjMzT1JHMkpAb3BzLW1vbmdvZGItZGV2MDEuc2hlaW4uY29tOjI3MDAxJyB9LApcIHsgJ25hbWUnOiAnd21zX3Rlc3QnLCAndXJsJzogJ215c3FsOi8vbHRfbmV3c2l0ZXRlc3RfcndfdGU6aDgwVExBMHVGZlhoSjFmU0AxMC4xMjMuNC4yMTozMzA4JyB9LApcIHsgJ25hbWUnOiAnd21kX3Rlc3QnLCAndXJsJzogJ215c3FsOi8vbHRfd21kX3Rlc3RfcndfdGU6NzV0a3BWbFNIM0FNZnExOEFjcFlAMTAuMTIzLjQuMjE6MzMwOCcgfSwKXCB7ICduYW1lJzogJ3d3c19ldXJfdGVzdCcsICd1cmwnOiAnbXlzcWw6Ly9sdF93d3NfZXVyX3Rlc3Rfcnc6d3dzaHVoYXdmNzg2MjhAMTAuMTIzLjMuNDA6NDAwNCcgfSwKXCB7ICduYW1lJzogJ3dtZF9ldXJfdGVzdCcsICd1cmwnOiAnbXlzcWw6Ly9sdF93bWRfZXVyX3Rlc3Rfcnc6d21kZHVoYXdmNzg5MmNAMTAuMTIzLjMuNDA6NDAwNCcgfSwKXCB7ICduYW1lJzogJ3dzc19ldXJfdGVzdCcsICd1cmwnOiAnbXlzcWw6Ly9sdF93c3NfdGVzdF9ldXJfcnc6dWh1aGF3Zjc4MTI0QDEwLjEyMy4zLjQwOjQwMDQnIH0sClwgeyAnbmFtZSc6ICd3d3NfbGFfdGVzdCcsICd1cmwnOiAnbXlzcWw6Ly9sdF93bXNfd2FyZWh3aXRoaW5fbGFfdGVzdF9ydzo5bXBkdThKdk1rUlU4OVZ6Q01iVkAxMC4xMjMuMy40MDo0MDAxJyB9LApcIHsgJ25hbWUnOiAnd3dzX25hX3Rlc3QnLCAndXJsJzogJ215c3FsOi8vbHRfd21zX25hX3Rlc3Rfcnc6ZG4yOTJzY2NmMzlTMlNERTNYRTNAMTAuMTIzLjMuNDA6NDAwNCcgfSwKXCB7ICduYW1lJzogJ3d3c19kZXZfcmVkaXMnLCAndXJsJzogJ3JlZGlzOi8vci0xOWUzMzY5YWZjZDI0MGE2LnJlZGlzLnNoZWlubWlkLmNvbTo5NzM2JyB9LApcIHsgJ25hbWUnOiAnd3dzX3Rlc3RfcmVkaXMnLCAndXJsJzogJ3JlZGlzOi8vci00NDdjN2Y4YTZiYWE0NGQ2LnJlZGlzLnNoZWlubWlkLmNvbTo5NzM2JyB9LApcIF0KYXV0b2NtZCBGaWxlVHlwZSBkYm91dCBzZXRsb2NhbCBub2ZvbGRlbmFibGUKYXV0b2NtZCBGaWxlVHlwZSBzcWwsbXlzcWwscGxzcWwgbHVhIHJlcXVpcmUoJ2NtcCcpLnNldHVwLmJ1ZmZlcih7IHNvdXJjZXMgPSB7eyBuYW1lID0gJ3ZpbS1kYWRib2QtY29tcGxldGlvbicgfX0gfSkKXV0K

local lazy_opts = {}
require("lazy").setup(plugins, lazy_opts)

-- Enable Comment.nvim
require('Comment').setup()

-- vim-easy-align
vim.keymap.set('n', '<Leader>Ea', ':EasyAlign<CR>', {})
vim.keymap.set('x', '<Leader>Ea', ':EasyAlign<CR>', {})

-- for ToggleTerm
require("toggleterm").setup {
  -- direction = 'horizontal', -- 'vertical' | 'horizontal' | 'tab' | 'float',
  -- size can be a number or function which is passed the current terminal
  size = function(term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
}
vim.keymap.set('n', '<C-\\><C-\\>', ":ToggleTerm<CR>", { desc = 'ToggleTerm' })

function _G.set_terminal_keymaps()
  local toggleterm_keymap_opts = { buffer = 0 }
  vim.keymap.set('t', '<C-\\><C-\\>', [[<Cmd>ToggleTerm<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-h>', [[<Cmd>wincmd h<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-j>', [[<Cmd>wincmd j<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-k>', [[<Cmd>wincmd k<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-l>', [[<Cmd>wincmd l<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-,>', [[<Cmd>-tabnext<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-.>', [[<Cmd>+tabnext<CR>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M-<>', [[<Cmd>-tabmove<CR><C-l>]], toggleterm_keymap_opts)
  vim.keymap.set('t', '<M->>', [[<Cmd>+tabmove<CR><C-l>]], toggleterm_keymap_opts)
end

-- if you only want these mappings for toggle term use term://*toggleterm#* instead
vim.cmd('autocmd! TermOpen term://* lua set_terminal_keymaps()')

-- lazygit using toggleterm
local Terminal = require('toggleterm.terminal').Terminal
-- If the hidden key is set to true, this terminal will not be toggled by normal toggleterm commands such as :ToggleTerm or the open mapping;  count=5 means lazygit terminal object can be specified with a count 5
local lazygit  = Terminal:new({ cmd = "lazygit", hidden = true, direction = 'float', count = 5 })
function _Lazygit_toggle()
  lazygit:toggle()
end

vim.keymap.set("n", "<leader>lg", "<cmd>lua _Lazygit_toggle()<CR>", { noremap = true, silent = true })

-- for snazzy
vim.g["SnazzyTransparent"] = 1
vim.cmd('colorscheme snazzy')
vim.cmd('hi Folded guifg=#d78787 guibg=None gui=bold')

-- -- for dracula/vim
-- vim.cmd('let g:dracula_colorterm = 0')  -- for transparency
-- vim.cmd('colorscheme dracula')
-- vim.cmd('hi Folded guifg=#d75f87 guibg=None gui=bold')

-- -- for catppuccin/nvim
-- require("catppuccin").setup({
--   transparent_background = true, -- disables setting the background color.
-- })

-- -- setup must be called before loading
-- vim.cmd.colorscheme "catppuccin"
-- vim.cmd('hi Folded guibg=None gui=bold')

-- Set lualine as statusline
-- See `:help lualine.txt`
require('lualine').setup({
  options = {
    section_separators = { left = '', right = '' },
    component_separators = { left = '|', right = '|' }
  }
})

-- for akinsho/bufferline.nvim
require("bufferline").setup{
  options = {
    diagnostics = "nvim_lsp",
    diagnostics_update_in_insert = false,
    diagnostics_indicator = function(count, level, diagnostics_dict, context)
      local s = " "
      for e, n in pairs(diagnostics_dict) do
        local sym = e == "error" and "  "
          or (e == "warning" and "  " or " 󰌶 " )
        s = s .. n .. sym
      end
      return s
    end,
    offsets = {
      { filetype = "NvimTree", text = "Explorer", highlight = "Directory", text_align = "center" },
    },
  },
}
vim.keymap.set('n', '<M-,>', ':BufferLineCyclePrev<CR>', options)
vim.keymap.set('n', '<M-.>', ':BufferLineCycleNext<CR>', options)
vim.keymap.set('n', '<M-<>', ':BufferLineMovePrev<CR>', options)
vim.keymap.set('n', '<M->>', ':BufferLineMoveNext<CR>', options)

-- norcalli/nvim-colorizer.lua
require 'colorizer'.setup()

-- Gitsigns
-- See `:help gitsigns.txt`
require('gitsigns').setup {
  -- current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
  -- current_line_blame_opts = {
  --   delay = 300,
  -- },
  -- current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map('n', ']c', function()
      if vim.wo.diff then return ']c' end
      vim.schedule(function() gs.next_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    map('n', '[c', function()
      if vim.wo.diff then return '[c' end
      vim.schedule(function() gs.prev_hunk() end)
      return '<Ignore>'
    end, {expr=true})

    -- Actions
    map('n', '<leader>hs', gs.stage_hunk)
    map('n', '<leader>hr', gs.reset_hunk)
    map('v', '<leader>hs', function() gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
    map('v', '<leader>hr', function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
    map('n', '<leader>hS', gs.stage_buffer)
    map('n', '<leader>hu', gs.undo_stage_hunk)
    map('n', '<leader>hR', gs.reset_buffer)
    map('n', '<leader>hp', gs.preview_hunk)
    map('n', '<leader>hb', function() gs.blame_line{full=true} end)
    map('n', '<leader>tb', gs.toggle_current_line_blame)
    map('n', '<leader>hd', gs.diffthis)
    map('n', '<leader>hD', function() gs.diffthis('~') end)
    map('n', '<leader>td', gs.toggle_deleted)

    -- Text object
    map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
  end
}

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup {
  defaults = {
    mappings = {
      i = {
        ['<C-u>'] = false,
        ['<C-d>'] = false,
        ["<esc>"] = "close",
      }
    }
  },
  pickers = {
    find_files = {
      theme = "dropdown",
      previewer = false,
      path_display = { "smart" }, -- :h telescope.defaults.path_display
    },
    live_grep = {
      theme = "dropdown",
      path_display = { "smart" },
    },
    buffers = {
      show_all_buffers = true,
      sort_lastused = true,
      theme = "dropdown",
      previewer = false,
      mappings = {
        i = {
          ["<c-k>"] = "delete_buffer",
        }
      },
      path_display = { "smart" },
    }
  }
}
-- https://github.com/nvim-telescope/telescope.nvim/blob/39b12d84e86f5054e2ed98829b367598ae53ab41/plugin/telescope.lua#L11-L91
-- telescope.lua#highlights
-- vim.api.nvim_set_hl(0, "TelescopBorder", {fg="#5E81AC"})
vim.api.nvim_set_hl(0, "TelescopeSelection", { bg = "None", bold = true })
vim.api.nvim_set_hl(0, "TelescopeMatching", { fg = "#ff6ac1", bold = true })

-- -- Enable telescope fzf native, if installed
-- pcall(require('telescope').load_extension, 'fzf')

-- telescope.nvim
vim.keymap.set('n', '<M-O>', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<M-F>', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<M-E>', require('telescope.builtin').buffers, { desc = '[S]earch [B]uffers' })
vim.keymap.set('n', '<M-R>', require('telescope.builtin').resume, { desc = '[S]earch Telescope [R]esume' })
vim.keymap.set('n', '<leader>sH', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })
vim.keymap.set('n', '<leader>sK', require('telescope.builtin').keymaps, { desc = '[S]earch [K]eymaps' })

-- -- vim-floaterm
-- -- " set floaterm window's background to black
-- vim.cmd[[hi floaterm guibg=black]]
-- -- " set floating window border line color to cyan, and background to orange
-- vim.cmd[[hi floatermborder guifg=cyan]]

-- nvim-autopairs
require('nvim-autopairs').setup({
  disable_filetype = { "TelescopePrompt", "vim" },
})

-- vim-startify
vim.keymap.set('n', '<Leader>\\', ':Startify<CR>', options)

-- undotree
vim.keymap.set('n', '<F5>', ':UndotreeToggle<CR>', options)

-- nvim-tree
vim.keymap.set('n', 'T', ':NvimTreeToggle<CR>', options)
vim.keymap.set('n', '<Leader>tf', ':NvimTreeFindFile<CR>', options)
vim.keymap.set('n', '<Leader>tr', ':NvimTreeRefresh<CR>', options)

-- vim-fugitive
vim.keymap.set('n', '<Leader>gb', ':Git blame<CR>', options)
vim.keymap.set('n', '<Leader>gc', ':G commit<CR>', options)
vim.keymap.set('n', '<Leader>gf', ':Git fetch<CR>', options)
vim.keymap.set('n', '<Leader>gF', ':Git pull<CR>', options)
vim.keymap.set('n', '<Leader>gp', ':Git push<CR>', options)
-- vim.keymap.set('n', '<Leader>gd', ':Gvdiffsplit<CR>', options)
vim.keymap.set('n', '<Leader>gd', ':DiffviewOpen<CR>', options) -- from diffview.nvim
vim.keymap.set('n', '<Leader>gh', ':diffget //2<CR>', options)
vim.keymap.set('n', '<Leader>gl', ':diffget //3<CR>', options)
vim.keymap.set('n', '<Leader>gs', ':G<CR>', options)

-- far
vim.keymap.set('n', '<Leader>F', ':F  %<left><left>', {})

-- diffview.nvim
require("diffview").setup({
  enhanced_diff_hl = true, -- See ':h diffview-config-enhanced_diff_hl'
})

-- IMPORTANT: make sure to setup neodev BEFORE lspconfig
require("neodev").setup({
  -- add any options here, or leave empty to use the default settings
})

require('mason').setup()
require('mason-lspconfig').setup({
  ensure_installed = {
    -- Replace these with whatever servers you want to install
    'bashls',
    'lua_ls',
    'pyright',  -- Pandas ExcelWriter Generates 'Cannot Instantiate Abstract Class' Error Incorrectly; https://github.com/microsoft/pylance-release/issues/386; pip install pandas-stubs; should solve this problem
    'clangd',
    'gopls',
    -- 'jdtls',
    -- 'rust_analyzer',
  },
  -- Whether servers that are set up (via lspconfig) should be automatically installed if they're not already installed.
  -- This setting has no relation with the `ensure_installed` setting.
  -- Can either be:
  --   - false: Servers are not automatically installed.
  --   - true: All servers set up via lspconfig are automatically installed.
  --   - { exclude: string[] }: All servers set up via lspconfig, except the ones provided in the list, are automatically installed.
  --       Example: automatic_installation = { exclude = { "rust_analyzer", "solargraph" } }
  ---@type boolean
  automatic_installation = false,
  handlers = {
    -- The first entry (without a key) will be the default handler
    -- and will be called for each installed server that doesn't have
    -- a dedicated handler.
    function(server_name) -- default handler (optional)
      require("lspconfig")[server_name].setup {}
    end,
    -- Next, you can provide a dedicated handler for specific servers.
    -- For example, a handler override for the `rust_analyzer`:
    ["jdtls"] = function()
      -- disable jdtls for setting up automatically, will be conflict with ftplugin/java.vim; otherwise will create 2 lsp clients;
    end
  }
})

-- Global mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = function(ev)
    -- Enable completion triggered by <c-x><c-o>
    vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

    -- Buffer local mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions
    local opts = { buffer = ev.buf }
    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
    vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
    vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, opts)
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, opts)
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, opts)
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, opts)
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>ca', vim.lsp.buf.code_action, opts)
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
    vim.keymap.set({ 'n', 'v' }, '<space>fm', function()
      vim.lsp.buf.format { async = true }
    end, opts)
  end,
})

-- from lua/icons.lua
local my_icons = require("icons")

-- lsp diagnostic setup
vim.diagnostic.config({
  -- virtual_text = true,
  virtual_text = {
    prefix = my_icons.diagnostics.Prefix,
  },
  signs = true,
  update_in_insert = false,
  underline = true,
  severity_sort = false,
  float = true,
})

for type, icon in pairs(my_icons.localsigns) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- nvim-treesitter
require 'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "c", "cpp", "make", "cmake", "dockerfile", "go", "lua", "python", "rust", "html",
    "javascript", "css", "toml", "vim", "json", "yaml" },

  -- Install languages synchronously (only applied to `ensure_installed`)
  sync_install = false,

  -- List of parsers to ignore installing
  -- ignore_install = { "javascript" },
  ignore_install = {},

  highlight = {
    -- `false` will disable the whole extension
    enable = true,

    -- NOTE: these are the names of the parsers and not the filetype. (for example if you want to
    -- disable highlighting for the `tex` filetype, you need to include `latex` in this list as this is
    -- the name of the parser)
    -- list of language that will be disabled
    -- disable = { "c", "rust" },
    -- Or use a function for more flexibility, e.g. to disable slow treesitter highlight for large files
    disable = function(lang, buf)
      local max_filesize = 100 * 1024 -- 100 KB
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

-- lsp_signature
local cfg = {} -- add your config here
require "lsp_signature".setup(cfg)

-- nvim-cmp
local kind_icons = my_icons.kind
local cmp = require('cmp')
-- luasnip load
local luasnip = require('luasnip')
require("luasnip.loaders.from_snipmate").lazy_load()
require('luasnip.loaders.from_vscode').lazy_load()
local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match "%s" == nil
end
cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ["<C-y>"] = cmp.config.disable,
    ["<C-n>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<C-p>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_locally_jumpable() then
        luasnip.expand_or_jump()
      elseif luasnip.jumpable(1) then
        luasnip.jump(1)
      elseif has_words_before() then
        -- cmp.complete()
        fallback()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
    ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
  }),
  sources = cmp.config.sources({
    { name = 'nvim_lsp', max_item_count = 20, group_index = 1 },
    -- { name = 'vsnip' }, -- For vsnip users.
    { name = 'luasnip',  max_item_count = 20, group_index = 1 }, -- For luasnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' }, -- For snippy users.
    -- { name = 'copilot', priority = 100, max_item_count = 3, group_index = 2 },
  }, {
    { name = 'buffer',     max_item_count = 6, group_index = 2, keyword_length = 3 },
    { name = "treesitter" },
    { name = "path" },
    { name = "spell" },
    { name = "cmp-cmdline" },
  }),
  formatting = {
    fields = { "abbr", "kind", "menu" },
    format = function(entry, vim_item)
      vim_item.kind = kind_icons[vim_item.kind] .. ' ' .. vim_item.kind
      vim_item.menu = ({
        nvim_lsp = "[LSP]",
        nvim_lua = "[NVIM_LUA]",
        luasnip = "[Snippet]",
        buffer = "[Buffer]",
        path = "[Path]",
        emoji = "[Emoji]",
        calc = "[Calc]",
        cmp_tabnine = "[Tabnine]",
        vsnip = "[Snippet]",
        tmux = "[TMUX]",
        copilot = "[Copilot]",
        treesitter = "[TreeSitter]",
      })[entry.source.name]
      return vim_item
    end,
  },
  window = {
    completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  -- experimental = {
  --   ghost_text = true,
  -- },
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline({ '/', '?' }, {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- https://github.com/windwp/nvim-autopairs#mapping-cr
-- https://github.com/hrsh7th/nvim-cmp/wiki/Advanced-techniques#nvim-autopairs
-- If you want insert `(` after select function or method item
local cmp_autopairs = require('nvim-autopairs.completion.cmp')
cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())

-- -- nvim-dap nvim-dap-ui
-- local dap, dapui = require("dap"), require("dapui")
-- dapui.setup({
-- })
--
-- local diagnostics_icons = my_icons.diagnostics
-- vim.fn.sign_define("DapBreakpoint",
--   { text = diagnostics_icons.Debug, texthl = "LspDiagnosticsSignError", linehl = "", numhl = "" })
-- vim.fn.sign_define("DapBreakpointCondition",
--   { text = diagnostics_icons.BoldQuestion, texthl = "LspDiagnosticsSignHint", linehl = "", numhl = "" })
-- vim.fn.sign_define("DapBreakpointRejected",
--   { text = diagnostics_icons.Reject, texthl = "LspDiagnosticsSignHint", linehl = "", numhl = "" })
-- vim.fn.sign_define("DapStopped",
--   {
--     text = diagnostics_icons.stoped,
--     texthl = "LspDiagnosticsSignInformation",
--     linehl = "DiagnosticUnderlineInfo",
--     numhl = "LspDiagnosticsSignInformation"
--   })
-- dap.listeners.after.event_initialized["dapui_config"] = function()
--   dapui.open()
-- end
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end
-- vim.keymap.set('n', '<F6>', require 'dap'.toggle_breakpoint)
-- vim.keymap.set('n', '<F14>',
--   function() require('dap').set_breakpoint(vim.fn.input('Breakpoint contidion: '), nil, nil) end) -- S-F6
-- vim.keymap.set('n', '<F7>', require 'dap'.step_into)
-- vim.keymap.set('n', '<F15>', require 'dap'.step_out)                                              -- S-F7
-- vim.keymap.set('n', '<F8>', require 'dap'.step_over)
-- vim.keymap.set('n', '<F9>', require 'dap'.continue)
-- vim.keymap.set('n', '<F21>', require 'dapui'.toggle) -- S-F9
--
-- require('dap-python').setup('/opt/homebrew/bin/python')
-- require('dap-go').setup()
-- dap.adapters.codelldb = {
--   type = 'server',
--   port = "${port}",
--   executable = {
--     -- CHANGE THIS to your path!
--     command = os.getenv("HOME") .. "/.vscode/extensions/vadimcn.vscode-lldb-1.9.2/adapter/codelldb",
--     args = { "--port", "${port}" },
--     -- args = {
--     -- "--port", "${port}",
--     --   function()
--     --     return vim.fn.input('Arguments: ')
--     --   end,
--     -- },
--
--     -- On windows you may have to uncomment this:
--     -- detached = false,
--   }
-- }
-- dap.configurations.cpp = {
--   {
--     name = "Launch file",
--     type = "codelldb",
--     request = "launch",
--     program = function()
--       return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
--     end,
--     -- args = {
--     --   function()
--     --     return vim.fn.input('Arguments: ')
--     --   end,
--     -- },
--     cwd = '${workspaceFolder}',
--     stopOnEntry = false,
--   },
-- }
-- dap.configurations.c = dap.configurations.cpp
-- dap.configurations.rust = dap.configurations.cpp

-- -- cmp-dap (nvim-cmp source for nvim-dap REPL and nvim-dap-ui buffers)
-- cmp.setup({
--   enabled = function()
--     return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
--         or require("cmp_dap").is_dap_buffer()
--   end
-- })
--
-- cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
--   sources = {
--     { name = "dap" },
--   },
-- })

-- for folke/noice.nvim
require("noice").setup({
  lsp = {
    -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
    override = {
      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
      ["vim.lsp.util.stylize_markdown"] = true,
      ["cmp.entry.get_documentation"] = true, -- requires hrsh7th/nvim-cmp
    },
  },
  -- you can enable a preset for easier configuration
  presets = {
    bottom_search = true, -- use a classic bottom cmdline for search
    command_palette = true, -- position the cmdline and popupmenu together
    long_message_to_split = true, -- long messages will be sent to a split
    inc_rename = false, -- enables an input dialog for inc-rename.nvim
    lsp_doc_border = false, -- add a border to hover docs and signature help
  },
})

-- for rcarriga/nvim-notify
require("notify").setup({
  background_colour = "#000000",
})

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
