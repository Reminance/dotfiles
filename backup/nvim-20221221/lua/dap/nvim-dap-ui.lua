local dap, dapui = require "dap", require "dapui"
dapui.setup({
  icons = { expanded = "▾", collapsed = "▸" },
  mappings = {
    -- Use a table to apply multiple mappings
    expand = { "<CR>", "<2-LeftMouse>" },
    open = "o",
    remove = "d",
    edit = "e",
    repl = "r",
    toggle = "t",
  },
  -- Expand lines larger than the window
  -- Requires >= 0.7
  expand_lines = vim.fn.has("nvim-0.7"),
  layouts = {
    {
      elements = {
        'scopes',
        'breakpoints',
        'stacks',
        'watches',
      },
      size = 40,
      position = 'left',
    },
    {
      elements = {
        'repl',
        'console',
      },
      size = 10,
      position = 'bottom',
    },
  },
  floating = {
    max_height = nil, -- These can be integers or a float between 0 and 1.
    max_width = nil, -- Floats will be treated as percentage of your screen.
    border = "single", -- Border style. Can be "single", "double" or "rounded"
    mappings = {
      close = { "q", "<Esc>" },
    },
  },
  windows = { indent = 1 },
  render = {
    max_type_length = nil, -- Can be integer or nil.
  }
})

local debug_open = function()
  -- 不开codelldb窗口
  -- dapui.open()
  dapui.open("sidebar")
  vim.api.nvim_command("DapVirtualTextEnable")
end
local debug_close = function()
  dap.repl.close()
  dapui.close()
  vim.api.nvim_command("DapVirtualTextDisable")
  -- vim.api.nvim_command("bdelete! term:")   -- close debug temrinal
end
dap.listeners.after.event_initialized["dapui_config"] = function()
  debug_open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  debug_close()
end
dap.listeners.before.event_exited["dapui_config"]     = function()
  debug_close()
end
dap.listeners.before.disconnect["dapui_config"]       = function()
  debug_close()
end