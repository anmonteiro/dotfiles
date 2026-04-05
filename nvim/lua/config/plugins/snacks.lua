return {
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    keys = {
      { "<leader>.", function() Snacks.scratch() end, desc = "Toggle scratch buffer" },
      { "<leader>S", function() Snacks.scratch.select() end, desc = "Select scratch buffer" },
      { "fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
      { "<leader>gg", function() Snacks.lazygit() end, desc = "Lazygit" },
      { "<leader>sh", function() Snacks.notifier.show_history() end, desc = "Notification history" },
      { "<leader>tt", function() Snacks.terminal.toggle() end, desc = "Toggle terminal" },
    },
    opts = {
      bigfile = { enabled = true },
      input = { enabled = true },
      notifier = { enabled = true },
      picker = { enabled = true },
      quickfile = { enabled = true },
      scratch = { enabled = true },
      terminal = { enabled = true },
      lazygit = { enabled = true },
    },
  },
}
