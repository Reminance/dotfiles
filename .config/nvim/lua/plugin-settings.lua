
-- sneak
vim.g["sneak#label"] = 1

-- rainbow
vim.g["rainbow_active"] = 1

-- colorscheme
-- material
-- vim.cmd('colorscheme material')

-- vim-snazzy
vim.g["SnazzyTransparent"] = 1
vim.cmd('colorscheme snazzy')
vim.cmd('highlight Special guifg=#ff6ac1 gui=italic,bold')  -- for telescope: highlight matching chars fg, gui=italic,underline,bold

-- onedarkpro
-- require('onedarkpro').load()
-- vim.cmd('colorscheme onedarkpro')

-- ayu-vim
-- vim.g["ayucolor"] = "dark" -- for dark version of theme
-- vim.cmd('colorscheme ayu')

-- tokyonight
-- vim.g.tokyonight_style = "night"
-- vim.cmd[[colorscheme tokyonight]]

-- dracula
-- vim.g.colorscheme_bg = "dark"
-- vim.cmd('colorscheme dracula')

-- nord-vim
-- vim.cmd('colorscheme nord')

-- vim-monokai
-- vim.cmd('colorscheme monokai')

-- tender
-- vim.g.NVIM_TUI_ENABLE_TRUE_COLOR=1
-- vim.cmd('colorscheme tender')

-- gruvbox-material
-- -- Set contrast.
-- vim.g.gruvbox_material_palette='material' -- material, mix, original
-- vim.g.gruvbox_material_background = 'hard' -- hard, medium, soft
-- vim.g.gruvbox_material_enable_bold=1
-- -- vim.g.gruvbox_material_enable_italic=1
-- vim.g.gruvbox_material_transparent_background=1
-- -- For better performance
-- vim.g.gruvbox_material_better_performance = 1
-- vim.cmd('colorscheme gruvbox-material')

-- sonokai
-- -- vim.g.sonokai_style = 'andromeda' -- 'default', 'atlantis', 'andromeda', 'shusia', 'maia', 'espresso'
-- vim.g.sonokai_style = 'default'
-- vim.g.sonokai_transparent_background=1
-- vim.g.sonokai_better_performance = 1
-- vim.cmd('colorscheme sonokai')

-- indentline
vim.cmd("let g:indentLine_char_list = ['|', '¦', '┆', '┊']")

-- nvim-tree.lua
require('nvim-tree').setup{
  view = {
    width = 30,
    height = 30,
    side = "left",
  },
  actions = {
    open_file = {
      resize_window = true
    }
  },
 }

-- nvim-autopairs
require('nvim-autopairs').setup({
  disable_filetype = { "TelescopePrompt" , "vim" },
})

-- Colorizer
vim.g.colorizer_auto_color = 1
-- vim.g.colorizer_auto_filetype = "yaml,zsh,zsh-theme,lua,vim,json"

-- bufferline.nvim
require("bufferline").setup{}

-- vim-floaterm
-- " Set floaterm window's background to black
vim.cmd[[hi Floaterm guibg=black]]
-- " Set floating window border line color to cyan, and background to orange
vim.cmd[[hi FloatermBorder guifg=cyan]]

-- toggleterm.nvim
require("toggleterm").setup{
  -- size can be a number or function which is passed the current terminal
  -- size = 20 | function(term)
  --   if term.direction == "horizontal" then
  --     return 15
  --   elseif term.direction == "vertical" then
  --     return vim.o.columns * 0.4
  --   end
  -- end,
  open_mapping = [[<c-\>]],
  -- on_open = fun(t: Terminal), -- function to run when the terminal opens
  -- on_close = fun(t: Terminal), -- function to run when the terminal closes
  -- on_stdout = fun(t: Terminal, job: number, data: string[], name: string) -- callback for processing output on stdout
  -- on_stderr = fun(t: Terminal, job: number, data: string[], name: string) -- callback for processing output on stderr
  -- on_exit = fun(t: Terminal, job: number, exit_code: number, name: string) -- function to run when terminal process exits
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  highlights = {
    -- highlights which map to a highlight group name and a table of it's values
    -- NOTE: this is only a subset of values, any group placed here will be set for the terminal window split
    -- Normal = {
    --   guibg = <VALUE-HERE>,
    -- },
    NormalFloat = {
      link = 'Normal'
    },
    -- FloatBorder = {
    --   guifg = <VALUE-HERE>,
    --   guibg = <VALUE-HERE>,
    -- },
  },
  shade_terminals = true,
  -- shading_factor = '<number>', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = true,
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  terminal_mappings = true, -- whether or not the open mapping applies in the opened terminals
  persist_size = true,
  -- direction = 'vertical' | 'horizontal' | 'tab' | 'float',
  direction = 'horizontal',
  close_on_exit = true, -- close the terminal window when the process exits
  shell = vim.o.shell, -- change the default shell
  -- This field is only relevant if direction is set to 'float'
  float_opts = {
    -- The border key is *almost* the same as 'nvim_open_win'
    -- see :h nvim_open_win for details on borders however
    -- the 'curved' border is a custom border type
    -- not natively supported but implemented in this plugin.
    -- border = 'single' | 'double' | 'shadow' | 'curved' | ... other options supported by win open
    border = 'single',
    -- width = <value>,
    -- height = <value>,
    winblend = 3,
  }
}

-- gitsigns.nvim
require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  signcolumn = true,  -- Toggle with `:Gitsigns toggle_signs`
  numhl      = false, -- Toggle with `:Gitsigns toggle_numhl`
  linehl     = false, -- Toggle with `:Gitsigns toggle_linehl`
  word_diff  = false, -- Toggle with `:Gitsigns toggle_word_diff`
  watch_gitdir = {
    interval = 1000,
    follow_files = true
  },
  attach_to_untracked = true,
  current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
  current_line_blame_opts = {
    virt_text = true,
    virt_text_pos = 'eol', -- 'eol' | 'overlay' | 'right_align'
    delay = 0,
    ignore_whitespace = false,
  },
  -- current_line_blame_formatter = '<author>, <author_time:%Y-%m-%d> - <summary>',
  current_line_blame_formatter = '        <author>, <author_time:%Y-%m-%d> • <summary>',
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  max_file_length = 40000,
  preview_config = {
    -- Options passed to nvim_open_win
    border = 'single',
    style = 'minimal',
    relative = 'cursor',
    row = 0,
    col = 1
  },
  yadm = {
    enable = false
  },
}

-- lualine.nvim
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    -- theme = 'gruvbox_dark',
    -- theme = 'gruvbox-material',
    -- component_separators = { left = "⦚", right = "  " },
    -- section_separators = { left = "", right = " " },
    -- component_separators = { left = '', right = ''},
    -- section_separators = { left = '', right = ''},
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {},
    always_divide_middle = true,
    globalstatus = false,
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff', 'diagnostics'},
    lualine_c = {
      { "filename", path = 1, symbols = { modified = "[+]", readonly = " " } },
      { "lsp_progress", display_components = { "lsp_client_name" } },
    },
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
}

-- statusline
vim.o.laststatus=3

-- lua-dev.nvim
local luadev = require("lua-dev").setup({
  -- add any options here, or leave empty to use the default settings
  -- lspconfig = {
  --   cmd = {"lua-language-server"}
  -- },
})
local lspconfig = require('lspconfig')
lspconfig.sumneko_lua.setup(luadev)

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

-- telescope-dap.nvim
require('telescope').load_extension('dap')

vim.cmd[[
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set foldlevel=99
function MyFoldText()
  let nucolwidth = &fdc + &number*&numberwidth
  let foldlinecount = foldclosedend(v:foldstart) - foldclosed(v:foldstart) + 1
  let winwd = winwidth(0) - nucolwidth - 2
  " let prefix = " _______>>> "
  " let fdnfo = prefix . string(v:foldlevel) . "," . string(foldlinecount)
  let fdnfo = "[" . string(v:foldlevel) . "," . string(foldlinecount) . "]"
  let line =  strpart(getline(v:foldstart), 0 , winwd - len(fdnfo))
  let fillcharcount = winwd - len(line) - len(fdnfo)
  return line . repeat(" ",fillcharcount) . fdnfo
endfunction
set foldtext=MyFoldText()
highlight Folded guibg=none guifg=cyan ctermbg=none
]]

