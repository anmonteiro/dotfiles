local use_snacks_statuscolumn = true

return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    keys = {
      {
        "<leader>.",
        function()
          Snacks.scratch()
        end,
        desc = "Toggle scratch buffer",
      },
      {
        "<leader>S",
        function()
          Snacks.scratch.select()
        end,
        desc = "Select scratch buffer",
      },
      {
        "fb",
        function()
          Snacks.picker.buffers()
        end,
        desc = "Buffers",
      },
      {
        "<leader>sh",
        function()
          vim.cmd("messages")
        end,
        desc = "Message history",
      },
      {
        "<leader>tt",
        function()
          Snacks.terminal.toggle()
        end,
        desc = "Toggle terminal",
      },
    },
    opts = {
      bigfile = { enabled = true },
      indent = {
        enabled = true,
        animate = {
          enabled = true,
          duration = {
            step = 10,
            total = 120,
          },
        },
        scope = { enabled = true },
      },
      input = { enabled = true },
      notifier = { enabled = false },
      picker = { enabled = true },
      quickfile = { enabled = true },
      scratch = { enabled = true },
      -- Flip this to false to fall back to Neovim's normal gutter.
      statuscolumn = { enabled = use_snacks_statuscolumn },
      terminal = { enabled = true },
      gitbrowse = { enabled = true },
    },
  },
}
