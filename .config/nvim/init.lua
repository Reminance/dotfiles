-- [[ Setting options ]]
-- See `:help vim.o`

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
vim.o.updatetime = 250
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

-- keymaps
vim.keymap.set("n", "Q", "<nop>")
vim.keymap.set("n", "R", "<nop>")
vim.keymap.set("n", "<C-q>", ":q<CR>")
vim.keymap.set("i", "<C-q>", "<ESC>:q<CR>")
vim.keymap.set("n", "<C-s>", ":w<CR>")
vim.keymap.set("i", "<C-s>", "<ESC>:w<CR>")
vim.keymap.set("i", "<C-n>", "<Down>")
vim.keymap.set("i", "<C-p>", "<Up>")
vim.keymap.set({ 'i', 'c' }, "<C-b>", "<Left>")
vim.keymap.set({ 'i', 'c' }, "<C-f>", "<Right>")
vim.keymap.set({ 'i', 'c' }, "<C-a>", "<Home>")
vim.keymap.set({ 'i', 'c' }, "<C-e>", "<End>")

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

-- Window Management
vim.keymap.set("n", "<M-h>", "<Esc><C-w>h")
vim.keymap.set("n", "<M-j>", "<Esc><C-w>j")
vim.keymap.set("n", "<M-k>", "<Esc><C-w>k")
vim.keymap.set("n", "<M-l>", "<Esc><C-w>l")

-- split the screens to up (horizontal), down (horizontal), left (vertical), right (vertical)
vim.keymap.set("n", "<Leader>sh", ":set nosplitright<CR>:vsplit<CR>")
vim.keymap.set("n", "<Leader>sj", ":set splitbelow<CR>:split<CR>")
vim.keymap.set("n", "<Leader>sk", ":set nosplitbelow<CR>:split<CR>")
vim.keymap.set("n", "<Leader>sl", ":set splitright<CR>:vsplit<CR>")

-- normal mode bindings
vim.keymap.set("n", "<S-Down>", ":res +1<CR>")
vim.keymap.set("n", "<S-Up>", ":res -1<CR>")
vim.keymap.set("n", "<S-Left>", ":vertical resize-1<CR>")
vim.keymap.set("n", "<S-Right>", ":vertical resize+1<CR>")

-- yank to system clipboard
vim.keymap.set("v", "Y", [["*y :let @+=@*<CR>]])

-- Rot13 encode {motion} text.
vim.keymap.set("v", "<Leader>xe", "mzg?`z")

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- go to last location when opening a buffer  " make cursor remain the position of last quit
vim.cmd[[au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif]]

-- syntax highlighting of search results
vim.cmd[[au ColorScheme * highlight Search guibg=NONE guifg=Cyan gui=italic,underline,bold]]

-- Don't move on *
vim.keymap.set("n", "*", "mm*`m", { noremap = true, silent = true })
-- Visual Mode * from Scrooloose
vim.cmd[[
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

-- Compile Function
vim.cmd[[
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
]]

-- Only required if you have packer configured as `opt`
vim.cmd.packadd('packer.nvim')

require('packer').startup(function(use)

  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

    -- Fuzzy Finder (files, lsp, etc)
  use { 'nvim-telescope/telescope.nvim', branch = '0.1.x', requires = { 'nvim-lua/plenary.nvim' } }
  -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
  use { 'nvim-telescope/telescope-fzf-native.nvim', run = 'make', cond = vim.fn.executable 'make' == 1 }

  use 'connorholyday/vim-snazzy'
  use({
	  'rose-pine/neovim',
	  as = 'rose-pine',
	  -- config = function()
		 --  vim.cmd('colorscheme rose-pine')
	  -- end
  })
  use { "ellisonleao/gruvbox.nvim" }
  use({'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'})
  use('mbbill/undotree')
  use('tpope/vim-fugitive')
  use 'lewis6991/gitsigns.nvim'
  use 'numToStr/Comment.nvim' -- "gc" to comment visual regions/lines
  use 'lukas-reineke/indent-blankline.nvim' -- Add indentation guides even on blank lines
  use 'nvim-lualine/lualine.nvim' -- Fancier statusline
  use 'mhinz/vim-startify'
  use {
    "folke/which-key.nvim",
    config = function()
      require("which-key").setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      }
    end
  }

  use 'junegunn/vim-easy-align'
  use 'tpope/vim-surround'

  -- icons
  use 'ryanoasis/vim-devicons'
  use 'kyazdani42/nvim-web-devicons'

  use {
    'kyazdani42/nvim-tree.lua',
    requires = {
      'kyazdani42/nvim-web-devicons', -- optional, for file icon
    },
    -- config = function() require'nvim-tree'.setup {} end
  }
  use 'luochen1990/rainbow'
  use 'windwp/nvim-autopairs'
  use 'chrisbra/Colorizer'

  use 'voldikss/vim-floaterm'

  use 'brooth/far.vim'

  -- file navigation
  use 'junegunn/fzf.vim'

  -- Draw ASCII diagrams in Neovim.
  use "jbyuki/venn.nvim"
  -- diffview
  use { 'sindrets/diffview.nvim', requires = 'nvim-lua/plenary.nvim' }

  use {
	  'VonHeikemen/lsp-zero.nvim',
	  requires = {
		  -- LSP Support
		  {'neovim/nvim-lspconfig'},
		  {'williamboman/mason.nvim'},
		  {'williamboman/mason-lspconfig.nvim'},

		  -- Autocompletion
		  {'hrsh7th/nvim-cmp'},
		  {'hrsh7th/cmp-buffer'},
		  {'hrsh7th/cmp-path'},
		  {'hrsh7th/cmp-cmdline'},
		  {'saadparwaiz1/cmp_luasnip'},
		  {'hrsh7th/cmp-nvim-lsp'},
		  {'hrsh7th/cmp-nvim-lua'},

		  -- Snippets
		  {'L3MON4D3/LuaSnip'},
		  {'rafamadriz/friendly-snippets'},
	  }
  }

  use({ "iamcco/markdown-preview.nvim", run = "cd app && npm install", setup = function() vim.g.mkdp_filetypes = { "markdown" } end, ft = { "markdown" }, })

end)

-- for snazzy
vim.g["SnazzyTransparent"] = 1
vim.cmd[[hi Special guifg=#ff6ac1 gui=italic,bold]]  -- for telescope: highlight matching chars fg, gui=italic,underline,bold
vim.cmd('colorscheme snazzy')

-- -- for rose-pine
-- require('rose-pine').setup({
--     disable_background = true
-- })

-- -- for gruvbox setup must be called before loading the colorscheme
-- require("gruvbox").setup({
--   undercurl = true,
--   underline = true,
--   bold = true,
--   italic = {
--     strings = true,
--     operators = true,
--     comments = true,
--     ...
--   },
--   strikethrough = true,
--   invert_selection = false,
--   invert_signs = false,
--   invert_tabline = false,
--   invert_intend_guides = false,
--   inverse = true, -- invert background for search, diffs, statuslines and errors
--   contrast = "", -- can be "hard", "soft" or empty string
--   palette_overrides = {},
--   overrides = {},
--   dim_inactive = false,
--   transparent_mode = true,
-- })
-- vim.cmd("colorscheme gruvbox")

-- Set lualine as statusline
-- See `:help lualine.txt`
require('lualine').setup {
  options = {
    icons_enabled = true,
    -- theme = 'onedark',
    component_separators = '|',
    section_separators = '',
  },
}

-- Enable Comment.nvim
require('Comment').setup()

-- Enable `lukas-reineke/indent-blankline.nvim`
-- See `:help indent_blankline.txt`
require('indent_blankline').setup {
  char = '┊',
  show_trailing_blankline_indent = false,
}

-- Colorizer
vim.g.colorizer_auto_color = 1
-- vim.g.colorizer_auto_filetype = "yaml,zsh,zsh-theme,lua,vim,json"

-- Gitsigns
-- See `:help gitsigns.txt`
require('gitsigns').setup {
  signs = {
    add = { text = '+' },
    change = { text = '~' },
    delete = { text = '_' },
    topdelete = { text = '‾' },
    changedelete = { text = '~' },
  },
}

-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
require('telescope').setup{
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
    -- find_files = {
    --   theme = "dropdown",
    -- },
    -- live_grep = {
    --   theme = "dropdown"
    -- },
    buffers = {
      show_all_buffers = true,
      sort_lastused = true,
      theme = "dropdown",
      previewer = false,
      mappings = {
        i = {
          ["<c-k>"] = "delete_buffer",
        }
      }
    }
  }
}


-- Enable telescope fzf native, if installed
pcall(require('telescope').load_extension, 'fzf')

-- telescope.nvim
vim.keymap.set('n', '<M-O>', require('telescope.builtin').find_files, { desc = '[S]earch [F]iles' })
vim.keymap.set('n', '<M-F>', require('telescope.builtin').live_grep, { desc = '[S]earch by [G]rep' })
vim.keymap.set('n', '<M-E>', require('telescope.builtin').buffers, { desc = '[S]earch [B]uffers' })
vim.keymap.set('n', '<leader>sH', require('telescope.builtin').help_tags, { desc = '[S]earch [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[S]earch current [W]ord' })
vim.keymap.set('n', '<leader>sd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics' })

-- nvim-autopairs
require('nvim-autopairs').setup({
  disable_filetype = { "TelescopePrompt" , "vim" },
})

-- rainbow
vim.g["rainbow_active"] = 1

-- nvim-tree.lua
require('nvim-tree').setup{
  view = {
    width = 30,
    side = "left",
  },
  actions = {
    open_file = {
      resize_window = true
    }
  },
 }

 -- vim-floaterm
-- " Set floaterm window's background to black
vim.cmd[[hi Floaterm guibg=black]]
-- " Set floating window border line color to cyan, and background to orange
vim.cmd[[hi FloatermBorder guifg=cyan]]

local options = { noremap = true, silent = true }

-- vim-startify
vim.keymap.set('n', '<Leader>\\', ':Startify<CR>', options)

-- vim-easy-align
vim.keymap.set('n', '<Leader>ea', '<Plug>EasyAlign', {})
vim.keymap.set('x', '<Leader>ea', '<Plug>EasyAlign', {})

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
vim.keymap.set('n', '<Leader>F', ':F  %<left><left>', options)

-- venn.nvim, Draw ASCII diagrams in Neovim: enable or disable keymappings
function _G.Toggle_venn()
    local venn_enabled = vim.inspect(vim.b.venn_enabled)
    if venn_enabled == "nil" then
        vim.b.venn_enabled = true
        vim.cmd[[setlocal ve=all]]
        -- draw a line on HJKL keystokes
        vim.api.nvim_buf_set_keymap(0, "n", "J", "<C-v>j:VBox<CR>", {noremap = true})
        vim.api.nvim_buf_set_keymap(0, "n", "K", "<C-v>k:VBox<CR>", {noremap = true})
        vim.api.nvim_buf_set_keymap(0, "n", "L", "<C-v>l:VBox<CR>", {noremap = true})
        vim.api.nvim_buf_set_keymap(0, "n", "H", "<C-v>h:VBox<CR>", {noremap = true})
        -- draw a box by pressing "f" with visual selection
        vim.api.nvim_buf_set_keymap(0, "v", "f", ":VBox<CR>", {noremap = true})
    else
        vim.cmd[[setlocal ve=]]
        vim.cmd[[mapclear <buffer>]]
        vim.b.venn_enabled = nil
    end
end
-- toggle keymappings for venn using <leader>v
vim.api.nvim_set_keymap('n', '<leader>V', ":lua Toggle_venn()<CR>", { noremap = true})

-- diffview.nvim
-- Lua
local actions = require("diffview.actions")

require("diffview").setup({
  enhanced_diff_hl = true, -- See ':h diffview-config-enhanced_diff_hl'
})

-- lsp config
vim.diagnostic.config({
  -- virtual_text = true,
  virtual_text = {
    prefix = '●', -- Could be '●', '■', '▎', 'x'
  },
  signs = true,
  underline = true,
  update_in_insert = false,
  severity_sort = false,
})

local signs = { Error = " ", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- nvim-treesitter
require'nvim-treesitter.configs'.setup {
  -- A list of parser names, or "all"
  ensure_installed = { "c", "cpp", "make", "cmake", "commonlisp", "dockerfile", "go", "java", "lua", "python", "rust", "html", "javascript", "css", "toml", "vim", "vue", "json", "yaml" },

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
    disable = {},

    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = false,
  },
}

-- lsp-zero
-- https://github.com/VonHeikemen/lsp-zero.nvim/blob/v1.x/doc/md/lsp.md#default-keybindings
-- K: Displays hover information about the symbol under the cursor in a floating window. See :help vim.lsp.buf.hover().
-- gd: Jumps to the definition of the symbol under the cursor. See :help vim.lsp.buf.definition().
-- gD: Jumps to the declaration of the symbol under the cursor. Some servers don't implement this feature. See :help vim.lsp.buf.declaration().
-- gi: Lists all the implementations for the symbol under the cursor in the quickfix window. See :help vim.lsp.buf.implementation().
-- go: Jumps to the definition of the type of the symbol under the cursor. See :help vim.lsp.buf.type_definition().
-- gr: Lists all the references to the symbol under the cursor in the quickfix window. See :help vim.lsp.buf.references().
-- <Ctrl-k>: Displays signature information about the symbol under the cursor in a floating window. See :help vim.lsp.buf.signature_help(). If a mapping already exists for this key this function is not bound.
-- <F2>: Renames all references to the symbol under the cursor. See :help vim.lsp.buf.rename().
-- <F4>: Selects a code action available at the current cursor position. See :help vim.lsp.buf.code_action().
-- gl: Show diagnostics in a floating window. See :help vim.diagnostic.open_float().
-- [d: Move to the previous diagnostic in the current buffer. See :help vim.diagnostic.goto_prev().
-- ]d: Move to the next diagnostic. See :help vim.diagnostic.goto_next().
local lsp = require('lsp-zero').preset({
  name = 'minimal',
  -- set_lsp_keymaps = true,
  set_lsp_keymaps = {preserve_mappings = false}, -- Change set_lsp_keymaps to this to force the keybindings from lsp-zero.
  manage_nvim_cmp = true,
  suggest_lsp_servers = false,
})
lsp.on_attach(function(client, bufnr)
  local opts = {buffer = bufnr}
  local bind = vim.keymap.set

  bind('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<cr>', opts)
  -- more keybindings...
end)
-- (Optional) Configure lua language server for neovim
lsp.nvim_workspace()
lsp.setup()
-- diagnostic setup
vim.diagnostic.config({
  virtual_text = true,
  signs = true,
  update_in_insert = false,
  underline = true,
  severity_sort = false,
  float = true,
})

-- nvim-cmp
local cmp = require('cmp')
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
    window = {
      -- completion = cmp.config.window.bordered(),
      -- documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
      -- { name = 'vsnip' }, -- For vsnip users.
      { name = 'luasnip' }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
      { name = "treesitter" },
      { name = "path" },
      { name = "spell" },
      { name = "cmp-cmdline" },
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

-- insert `(` after select function or method item
local cmp_autopairs = require "nvim-autopairs.completion.cmp"
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
