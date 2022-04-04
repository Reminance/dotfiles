-- ensure packer is installed
-- https://github.com/wbthomason/packer.nvim#bootstrapping
local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

-- sync plugins on write/save
vim.cmd([[
augroup SyncPackerPlugins
autocmd!
autocmd BufWritePost plugins.lua source <afile> | PackerSync
augroup END
]])

return require('packer').startup {
  function(use)
    -- package manager
    use 'wbthomason/packer.nvim'

    -- color scheme
    -- use 'kaicataldo/material.vim'
    use 'connorholyday/vim-snazzy'

    -- icons
    use 'ryanoasis/vim-devicons'
    use 'kyazdani42/nvim-web-devicons'

    -- util
    use 'nvim-lua/plenary.nvim'
    use 'nvim-lua/popup.nvim'
    use 'mhinz/vim-startify'
    use 'justinmk/vim-sneak'
    use 'majutsushi/tagbar'
    use 'Yggdroot/indentLine'
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

    -- version control
    use 'tpope/vim-fugitive'
    use {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
    }

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
