return {
  "christoomey/vim-tmux-navigator",
  cmd = {
    "TmuxNavigateLeft",
    "TmuxNavigateDown",
    "TmuxNavigateUp",
    "TmuxNavigateRight",
    "TmuxNavigatePrevious",
  },
  keys = {
    { "<M-h>", "<cmd>TmuxNavigateLeft<cr>", mode = { "n", "t" }, silent = true },
    { "<M-j>", "<cmd>TmuxNavigateDown<cr>", mode = { "n", "t" }, silent = true },
    { "<M-k>", "<cmd>TmuxNavigateUp<cr>", mode = { "n", "t" }, silent = true },
    { "<M-l>", "<cmd>TmuxNavigateRight<cr>", mode = { "n", "t" }, silent = true },
    { "<M-\\>", "<cmd>TmuxNavigatePrevious<cr>", mode = { "n", "t" }, silent = true },
  },
  init = function()
    vim.g.tmux_navigator_no_mappings = true
  end,
}
