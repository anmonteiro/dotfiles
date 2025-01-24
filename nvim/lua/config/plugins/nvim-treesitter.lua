return {
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      { "reasonml-editor/tree-sitter-reason" },
    },
    build = ":TSUpdate",
    event = { "BufEnter" },
    -- lazy = false,
    config = function()
      require("config.treesitter").setup()
    end,
  },
}
