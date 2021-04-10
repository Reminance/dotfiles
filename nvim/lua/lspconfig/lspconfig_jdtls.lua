local M = {}
function M.setup()
    -- vim.cmd[[set softtabstop=4]]
    -- vim.cmd[[set shiftwidth=4]]
    -- vim.cmd[[set expandtab]]
    require('jdtls').start_or_attach({cmd = {'nvim-lspconfig-jdtls.sh'}})

    -- find_root looks for parent directories relative to the current buffer containing one of the given arguments.
    -- require('jdtls').start_or_attach({cmd = {'java-lsp.sh'}, root_dir = require('jdtls.setup').find_root({'gradle.build', 'pom.xml'})})

    -- Utility servers
    local map = function(type, key, value)
        vim.api.nvim_buf_set_keymap(0,type,key,value,{noremap = true, silent = true});
    end
    -- GOTO mappings
    map('n','gD','<Cmd>lua vim.lsp.buf.declaration()<CR>')
    map('n','gd','<Cmd>lua vim.lsp.buf.definition()<CR>')
    map('n','K','<Cmd>lua vim.lsp.buf.hover()<CR>')
    map('n','gr','<Cmd>lua vim.lsp.buf.references()<CR>')
    map('n','gs','<Cmd>lua vim.lsp.buf.signature_help()<CR>')
    map('n','gi','<Cmd>lua vim.lsp.buf.implementation()<CR>')
    map('n','gt','<Cmd>lua vim.lsp.buf.type_definition()<CR>')
    map('n','gw','<Cmd>lua vim.lsp.buf.document_symbol()<CR>')
    map('n','gW','<Cmd>lua vim.lsp.buf.workspace_symbol()<CR>')
    -- ACTION mappings
    map('n','<Leader>ah',  '<Cmd>lua vim.lsp.buf.hover()<CR>')
    map('n','<A-CR>', '<Cmd>lua require"jdtls".code_action()<CR>')
    map('n','<Leader>ar',  '<Cmd>lua vim.lsp.buf.rename()<CR>')
    -- Few language severs support these three
    map('n','<Leader>a=',  '<Cmd>lua vim.lsp.buf.formatting()<CR>')
    map('n','<Leader>ai',  '<Cmd>lua vim.lsp.buf.incoming_calls()<CR>')
    map('n','<Leader>ao',  '<Cmd>lua vim.lsp.buf.outgoing_calls()<CR>')
    -- Diagnostics mapping
    map('n','<Leader>ee', '<Cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')
    map('n','<Leader>en', '<Cmd>lua vim.lsp.diagnostic.goto_next()<CR>')
    map('n','<Leader>ep', '<Cmd>lua vim.lsp.diagnostic.goto_prev()<CR>')

    map('n', [[<Leader>ai]], [[<Cmd>lua require'jdtls'.organize_imports()<CR>]])
    map('n', [[<Leader>av]], [[<Cmd>lua require('jdtls').extract_variable()<CR>]])
    map('v', [[<Leader>av]], [[<Esc><Cmd>lua require('jdtls').extract_variable(true)<CR>]])
    map('v', [[<Leader>am]], [[<Esc><Cmd>lua require('jdtls').extract_method(true)<CR>]])
    map('n', [[<Leader>aR]], [[<Cmd>lua require('jdtls').code_action(false, 'refactor')<CR>]])

    -- If using nvim-dap
    -- This requires java-debug and vscode-java-test bundles, see install steps in this README further below.
    -- nnoremap <leader>df <Cmd>lua require'jdtls'.test_class()<CR>
    -- nnoremap <leader>dn <Cmd>lua require'jdtls'.test_nearest_method()<CR>

    vim.cmd[[command! -buffer JdtCompile lua require('jdtls').compile()]]
    vim.cmd[[command! -buffer JdtUpdateConfig lua require('jdtls').update_project_config()]]
    vim.cmd[[command! -buffer JdtJol lua require('jdtls').jol()]]
    vim.cmd[[command! -buffer JdtBytecode lua require('jdtls').javap()]]
    vim.cmd[[command! -buffer JdtJshell lua require('jdtls').jshell()]]

    -- local jdtls_ui = require'jdtls.ui'
    -- function jdtls_ui.pick_one_async(items, _, _, cb)
    --     require'lsputil.codeAction'.code_action_handler(nil, nil, items, nil, nil, nil, cb)
    -- end
    -- vim.g.lsp_utils_location_opts = {
    --     height = 24,
    --     mode = 'split',
    --     list = {
    --         border = true,
    --         numbering = true
    --     },
    --     preview = {
    --         title = 'Location Preview',
    --         border = true,
    --     },
    -- }

    -- vim.g.lsp_utils_symbols_opts = {
    --     height = 24,
    --     mode = 'editor',
    --     list = {
    --         border = true,
    --         numbering = false,
    --     },
    --     preview = {
    --         title = 'Symbols Preview',
    --         border = true,
    --     },
    --     prompt = {}
    -- }
    -- vim.lsp.handlers['textDocument/codeAction'] = require'lsputil.codeAction'.code_action_handler
    -- vim.lsp.handlers['textDocument/references'] = require'lsputil.locations'.references_handler
    -- vim.lsp.handlers['textDocument/definition'] = require'lsputil.locations'.definition_handler
    -- vim.lsp.handlers['textDocument/declaration'] = require'lsputil.locations'.declaration_handler
    -- vim.lsp.handlers['textDocument/typeDefinition'] = require'lsputil.locations'.typeDefinition_handler
    -- vim.lsp.handlers['textDocument/implementation'] = require'lsputil.locations'.implementation_handler
    -- vim.lsp.handlers['textDocument/documentSymbol'] = require'lsputil.symbols'.document_handler
    -- vim.lsp.handlers['workspace/symbol'] = require'lsputil.symbols'.workspace_handler
end

return M
