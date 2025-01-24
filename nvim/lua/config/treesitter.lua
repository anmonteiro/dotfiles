local M = {}

M.setup = function()
  require("nvim-treesitter.configs").setup({
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
      enable = true,
    },
  })

  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = "TSUpdate",
    callback = function()
      local parsers = require("nvim-treesitter.parsers")

      parsers.reason = {
        -- tier = 0,
        install_info = {
          url = "https://github.com/reasonml-editor/tree-sitter-reason",
          files = { "src/parser.c", "src/scanner.c" },
          branch = "master",
        },
      }
    end,
  })
end

return M
