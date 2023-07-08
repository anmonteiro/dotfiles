
local _ = require("nvim-treesitter.configs").setup {
  ensure_installed = {
    "graphql",
    "go",
    "html",
    "javascript",
    "json",
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
