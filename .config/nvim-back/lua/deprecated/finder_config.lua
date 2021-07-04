-- local map = require'mappings'
-- local finder = require'finder'
-- local M = {}

-- local function finderMap(key, func, str)
--     str = str or ''
--     local cmd = string.format([[<cmd>lua require'finder_config'.%s(%s)<CR>]], func, str)
--     map.nmap(key, cmd)
-- end

-- local function finderDirectMap(key, func, str)
--     str = str or ''
--     local cmd = string.format([[<cmd>lua require'finder'.%s(%s)<CR>]], func, str)
--     map.nmap(key, cmd)
-- end

-- function M.git_files(str)
--     finder.git_files{
--         preview_disabled = true,
--         init_text = str
--     }
-- end

-- function M.files(str)
--     finder.files{
--         init_text = str,
--     }
-- end

-- function M.fuzzy_grep(str)
--     finder.fuzzy_grep{
--         init_text = str,
--     }
-- end

-- function M.buffers(str)
--     finder.buffers{
--         init_text = str,
--     }
-- end

-- function M.grep(str)
--     finder.grep{
--         init_text = str,
--     }
-- end

-- function M.help(str)
--     finder.help_tags{
--         height = 30,
--         init_text = str,
--     }
-- end

-- function M.colors(str)
--     finder.colorschemes{
--         height = 30,
--         init_text = str,
--     }
-- end

-- function M.files_nvim(str)
--     finder.files{
--         init_text = str,
--         height = 30,
--         preview_disabled = true,
--         cwd = '/home/rishabh/.config/nvim'
--     }
-- end

-- finderMap('<Leader>ff', 'files')
-- finderMap('<Leader>fn', 'files_nvim')
-- finderMap('<Leader>fgf', 'git_files')
-- finderMap('<Leader>fl', 'fuzzy_grep')
-- finderMap('<Leader>fb', 'buffers')
-- finderMap('<Leader>fL', 'grep')
-- finderMap('<A-y>', 'help')
-- finderMap('<C-y>', 'colors')
-- finderDirectMap('<Leader>fk', 'buffer_lines')
-- finderDirectMap('<Leader>fm', 'man_pages')
-- finderDirectMap('<Leader>fc', 'finder_commands')

-- return M
