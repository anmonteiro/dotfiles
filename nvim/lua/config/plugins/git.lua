return {
  "tpope/vim-fugitive",
  {
    "mhinz/vim-signify",
    init = function()
      vim.g.signify_vcs_list = { "git" }
      vim.g.signify_realtime = 0
      vim.g.signify_sign_add = "⨁"
      vim.g.signify_sign_change = "✎"
      vim.g.signify_sign_delete = "✖"
      vim.cmd("highlight SignColumn ctermbg=NONE guibg=NONE")
    end,
  },
}
