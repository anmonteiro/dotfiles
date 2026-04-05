return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    dependencies = {
      { "reasonml-editor/tree-sitter-reason" },
    },
    build = function()
      require("config.treesitter").build()
    end,
    lazy = false,
    config = function()
      require("config.treesitter").setup()
    end,
  },
}
