
local M =  {}

M.setup = function()
  local list = require("nvim-treesitter.parsers").get_parser_configs()
  list.reason = {
    install_info = {
      url = "https://github.com/reasonml-editor/tree-sitter-reason",
      files = { "src/parser.c", "src/scanner.c" },
      branch = "master",
    },
  }

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
      "reason",
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
