return {
  {
    "neovim/nvim-lspconfig",
    config = function()
      require("config.lsp").setup()
    end,
  },
}
