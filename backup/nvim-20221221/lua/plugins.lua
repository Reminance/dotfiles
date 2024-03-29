-- ensure packer is installed
-- https://github.com/wbthomason/packer.nvim#bootstrapping
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  PACKER_BOOTSTRAP = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

-- sync plugins on write/save
-- vim.cmd([[
-- augroup SyncPackerPlugins
-- autocmd!
-- autocmd BufWritePost plugins.lua source <afile> | PackerSync
-- augroup END
-- ]])

return require('packer').startup {
  function(use)
    -- package manager
    use 'wbthomason/packer.nvim'

    -- color scheme
    -- use 'kaicataldo/material.vim'
    use 'connorholyday/vim-snazzy'
    -- use 'olimorris/onedarkpro.nvim'
    -- use 'ayu-theme/ayu-vim'
    -- use 'folke/tokyonight.nvim'
    -- use 'dracula/vim'
    -- use 'arcticicestudio/nord-vim'
    -- use 'sickill/vim-monokai'
    -- use 'jacoborus/tender.vim'
    -- use 'sainnhe/gruvbox-material'
    -- use 'sainnhe/sonokai'

    -- icons
    use 'ryanoasis/vim-devicons'
    use 'kyazdani42/nvim-web-devicons'

    -- util
    use 'nvim-lua/plenary.nvim'
    use 'nvim-lua/popup.nvim'
    use 'mhinz/vim-startify'
    -- use 'justinmk/vim-sneak'
    use {
      'phaazon/hop.nvim',
      branch = 'v1', -- optional but strongly recommended
      config = function()
        -- you can configure Hop the way you like here; see :h hop-config
        require'hop'.setup { keys = 'etovxqpdygfblzhckisuran' }
      end
    }
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
    use 'ludovicchabant/vim-gutentags' -- for gtags
    use 'majutsushi/tagbar'
    use 'Yggdroot/indentLine'
    use 'junegunn/vim-easy-align'
    use 'tpope/vim-surround'
    use 'tpope/vim-commentary'
    use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install' }
    use 'mbbill/undotree'
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
    use {'akinsho/bufferline.nvim', requires = 'kyazdani42/nvim-web-devicons'}
    use {
      'nvim-lualine/lualine.nvim',
      requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    }
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate', }
    use 'voldikss/vim-floaterm'
    use 'akinsho/toggleterm.nvim'
    use {
      "folke/trouble.nvim",
      config = function()
        require("trouble").setup {}
      end
    }
    use "nvim-pack/nvim-spectre" -- search and replace pane

    -- version control
    use 'tpope/vim-fugitive'
    use {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
    }
    use 'sindrets/diffview.nvim'

    -- file navigation
    use 'junegunn/fzf.vim'
    -- 🔭telescope
    use {
      'nvim-telescope/telescope.nvim',
      requires = { {'nvim-lua/plenary.nvim'} }
    }

    -- lsp
    use 'neovim/nvim-lspconfig'
    use 'folke/lua-dev.nvim'
    use 'williamboman/nvim-lsp-installer'
    -- use 'glepnir/lspsaga.nvim'
    use 'onsails/lspkind-nvim'
    use 'ray-x/lsp_signature.nvim'
    use 'arkav/lualine-lsp-progress'

    -- nvim-dap
    use { "rcarriga/nvim-dap-ui", requires = {"mfussenegger/nvim-dap"} }
    use "nvim-telescope/telescope-dap.nvim"
    use "theHamsta/nvim-dap-virtual-text"

    -- completion
    use {
      "hrsh7th/nvim-cmp",
      requires = {
        { "hrsh7th/cmp-buffer" },
        { "hrsh7th/cmp-nvim-lsp" },
        { "hrsh7th/cmp-path" },
        { "hrsh7th/cmp-cmdline" },
        { "hrsh7th/cmp-nvim-lua" },
        { "ray-x/cmp-treesitter" },
        { "hrsh7th/cmp-vsnip" },
        { "hrsh7th/vim-vsnip" },
      },
      config = function()
        require "nvim-completion"
      end,
    }

    -- setup config after cloning packer
    if PACKER_BOOTSTRAP then
      require('packer').sync()
    end
  end,
  config = {
    display = {
      open_fn = require('packer.util').float,
    },
  },
}
