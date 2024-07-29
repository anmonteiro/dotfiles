
local _ = require("nvim-treesitter.configs").setup {
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
    -- my own...
    "rapper",
  },
  highlight = {
    enable = true
  }
}


require("ocaml").setup()
