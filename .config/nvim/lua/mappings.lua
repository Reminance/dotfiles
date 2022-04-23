local map = vim.api.nvim_set_keymap

options = { noremap = true, silent = true }

-- ** Key Mappings ***

-- vim-startify
map('n', '<Leader>\\', ':Startify<CR>', options)

-- vim-sneak
map('n', 's', '<Plug>Sneak_s', {})
map('n', 'S', '<Plug>Sneak_S', {})

-- vim-easy-align
map('n', 'ga', '<Plug>EasyAlign', {})
map('x', 'ga', '<Plug>EasyAlign', {})

-- tagbar
map('n', '<Leader>T', ':TagbarOpenAutoClose<CR>', options)

-- undotree
map('n', '<F5>', ':UndotreeToggle<CR>', options)

-- nvim-tree
map('n', 'T', ':NvimTreeToggle<CR>', options)

-- vim-fugitive
map('n', '<Leader>gb', ':Git blame<CR>', options)
map('n', '<Leader>gc', ':G commit<CR>', options)
map('n', '<Leader>gf', ':Git fetch<CR>', options)
map('n', '<Leader>gF', ':Git pull<CR>', options)
map('n', '<Leader>gp', ':Git push<CR>', options)
map('n', '<Leader>gd', ':Gvdiffsplit<CR>', options)
map('n', '<Leader>gh', ':diffget //2<CR>', options)
map('n', '<Leader>gl', ':diffget //3<CR>', options)
map('n', '<Leader>gs', ':G<CR>', options)

-- -- fzf.vim
-- map('n', '<M-S-l>', ':Lines<CR>', options)
-- map('n', '<M-S-f>', ':Rg<CR>', options)
-- map('n', '<M-S-a>', ':Ag<CR>', options)
-- map('n', '<M-S-g>', ':GFiles<CR>', options)
-- map('n', '<M-S-d>', ':GFiles?<CR>', options)
-- map('n', '<M-S-o>', ':Files<CR>', options)
-- map('n', '<M-S-e>', ':Buffers<CR>', options)
-- map('n', '<M-S-h>', ':History<CR>', options)
-- map('n', '<M-S-t>', ':BTags<CR>', options)
-- map('n', '<M-S-c>', ':BCommits<CR>', options)
-- vim.g.fzf_preview_window='right:60%'
-- vim.g.fzf_commits_log_options='--graph --color=always --format="%C(auto)%h%d %s %C(blue)%C(bold)%cr"'

-- telescope.nvim
map('n', '<M-S-f>', '<cmd>Telescope live_grep<cr>', options)
map('n', '<M-S-o>', '<cmd>Telescope find_files theme=dropdown previewer=false<cr>', options) -- theme: default(null) dropdown cursor ivy
map('n', '<M-S-e>', '<cmd>Telescope buffers<cr>', options)
map('n', '<M-S-h>', '<cmd>Telescope help_tags<cr>', options)
require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = "close"
      }
    }
  }
}

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
vim.api.nvim_set_keymap('n', '<space>dl', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)

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

-- vim.cmd[[set signcolumn=yes]]
local signs = { Error = " ", Warn = "", Hint = "", Info = "" }
for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- Show line diagnostics automatically in hover window
-- vim.cmd [[autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})]] -- don't automatically popup diagnostic window
-- For diagnostics for specific cursor position
-- vim.cmd [[autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false, scope="cursor"})]]

-- nvim-lspconfig
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)

  -- lsp-signature setup
  require("lsp_signature").on_attach {
      bind = true, -- This is mandatory, otherwise border config won't get registered.
      doc_lines = 2, -- will show 2 lines of comment/doc(if there are more than 2 lines in doc, will be truncated)
      -- set to 0 if you DO NOT want any API comments be shown
      -- This setting only take effect in insert mode, it does not affect signature help in normal
      -- mode, 10 by default

      floating_window = true, -- show hint in a floating window, set to false for virtual text only mode
      hint_enable = true,
      hint_prefix = "🌟 ",
      hint_scheme = "String",
      use_lspsaga = false,
      hi_parameter = "PmenuSel", -- hl-search
      max_height = 12, -- max height of signature floating_window, if content is more than max_height, you can scroll down
      -- to view the hiding contents
      max_width = 120, -- max_width of signature floating_window, line will be wrapped if exceed max_width
      handler_opts = {
        border = "single", -- double, single, shadow, none
      },
      extra_trigger_chars = {}, -- Array of extra characters that will trigger signature completion, e.g., {"(", ","}
    }

  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<space>lf', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
end

-- snippet support
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- local servers = { 'bashls', 'clangd','gopls', 'pyright', 'rust_analyzer', 'tsserver', 'sumneko_lua' }
local servers = { 'bashls', 'clangd','gopls', 'pyright', 'rust_analyzer', 'tsserver' }
for _, lsp in pairs(servers) do
  require('lspconfig')[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    }
  }
end

-- vsnip jump through snippets with <Tab>
map("i", "<Tab>", [[vsnip#jumpable(1) ? '<Plug>(vsnip-jump-next)' : '<Tab>']], { noremap = false, expr = true })
map("s", "<Tab>", [[vsnip#jumpable(1) ? '<Plug>(vsnip-jump-next)' : '<Tab>']], { noremap = false, expr = true })
map( "i", "<S-Tab>", [[vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>']], { noremap = false, expr = true })
map( "s", "<S-Tab>", [[vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>']], { noremap = false, expr = true })

