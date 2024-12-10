return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      { "reasonml-editor/tree-sitter-reason" },
    },

    build = function() require("nvim-treesitter.install").update { with_sync = true } end,
    event = { "BufEnter" },
    lazy = false,
    config = function()
      require("config.treesitter").setup()
    end,
  },
}
