-- nvim-dap
-- local dap = require('dap')
local dap_breakpoint = {
  error = {
    -- text = "🧘🛑⊚⭕🟢🔵🚫👉⭐️⛔️🔴",
    text = "🔴",
    texthl = "LspDiagnosticsSignError",
    linehl = "",
    numhl = "",
  },
  rejected = {
    text = "",
    texthl = "LspDiagnosticsSignHint",
    linehl = "",
    numhl = "",
  },
  stopped = {
    text = "⭐️",
    texthl = "LspDiagnosticsSignInformation",
    linehl = "DiagnosticUnderlineInfo",
    numhl = "LspDiagnosticsSignInformation",
  },
}
vim.fn.sign_define("DapBreakpoint", dap_breakpoint.error)
vim.fn.sign_define("DapStopped", dap_breakpoint.stopped)
vim.fn.sign_define("DapBreakpointRejected", dap_breakpoint.rejected)


-- auto load saved breakpoints
vim.cmd[[
augroup _load_break_points
  autocmd!
  autocmd FileType c,cpp,go,python,lua :lua require('dap.dap-util').load_breakpoints()
augroup end
]]
