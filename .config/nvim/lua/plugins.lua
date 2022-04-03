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
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-surround'
    use 'tpope/vim-fugitive'
    use { 'iamcco/markdown-preview.nvim', run = 'cd app && yarn install' }
    use 'mbbill/undotree'
    use 'neovim/nvim-lspconfig'
    use 'williamboman/nvim-lsp-installer'
    use 'onsails/lspkind-nvim'
     -- nvim-cmp
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
        require "nvim-cmp"
      end,
    }
    -- 🔭telescope
    use 'nvim-telescope/telescope.nvim'
    use 'windwp/nvim-autopairs'
    use 'nvim-lua/popup.nvim'
    use 'nvim-lua/lsp-status.nvim'
    use 'folke/lua-dev.nvim'
    use 'ray-x/lsp_signature.nvim'
    use {
      'nvim-treesitter/nvim-treesitter',
      run = ':TSUpdate',
    }
    use 'chrisbra/Colorizer'
    use 'nvim-lua/plenary.nvim'
    use 'kyazdani42/nvim-web-devicons'
    use {
      'nvim-lualine/lualine.nvim',
      config = function()
        -- require 'joel.statusline'
      end,
      requires = { 'kyazdani42/nvim-web-devicons', opt = true },
    }
    use 'arkav/lualine-lsp-progress'
    use {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
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
