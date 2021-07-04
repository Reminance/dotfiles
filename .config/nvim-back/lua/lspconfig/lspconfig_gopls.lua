local M = {}
function M.setup()
    local lspconfig = require'lspconfig'
    lspconfig.gopls.setup {
        cmd = {"gopls", "serve"},
        settings = {
            gopls = {
                analyses = {
                    unusedparams = true,
                },
                staticcheck = true,
            },
        },
    }
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

    function goimports(timeoutms)
        local context = { source = { organizeImports = true } }
        vim.validate { context = { context, "t", true } }

        local params = vim.lsp.util.make_range_params()
        params.context = context

        -- See the implementation of the textDocument/codeAction callback
        -- (lua/vim/lsp/handler.lua) for how to do this properly.
        local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, timeout_ms)
        if not result or next(result) == nil then return end
        local actions = result[1].result
        if not actions then return end
        local action = actions[1]

        -- textDocument/codeAction can return either Command[] or CodeAction[]. If it
        -- is a CodeAction, it can have either an edit, a command or both. Edits
        -- should be executed first.
        if action.edit or type(action.command) == "table" then
            if action.edit then
                vim.lsp.util.apply_workspace_edit(action.edit)
            end
            if type(action.command) == "table" then
                vim.lsp.buf.execute_command(action.command)
            end
        else
            vim.lsp.buf.execute_command(action)
        end
    end
end

return M
