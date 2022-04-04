-- statusline
-- vim.o.laststatus=0

-- sneak
vim.g["sneak#label"] = 1

-- rainbow
vim.g["rainbow_active"] = 1

-- colorscheme
-- vim.cmd('colorscheme material')
vim.g["SnazzyTransparent"] = 1
vim.cmd('colorscheme snazzy')

-- indentline
vim.g["indentLine_char_list"] = "['|', '¦', '┆', '┊']"

-- nvim-tree.lua
require('nvim-tree').setup{}

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

-- lualine.nvim
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'auto',
    component_separators = { left = '', right = ''},
    section_separators = { left = '', right = ''},
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
  -- One of "all", "maintained" (parsers with maintainers), or a list of languages
  ensure_installed = "maintained",

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


