
local M =  {}

M.setup = function()
  require("nvim-treesitter.configs").setup {
    ensure_installed = {
      "c",
      "graphql",
      "go",
      "html",
      "javascript",
      "json",
      "jsonc",
      "lua",
      "markdown",
      "nix",
      "ocaml",
      "ocaml_interface",
      "python",
      "query",
      "rust",
      "toml",
      "tsx",
      "typescript",
      "vim",
    },
    highlight = {
      enable = true
    }
  }
end

return M
