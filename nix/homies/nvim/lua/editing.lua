
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

local fmt_group = vim.api.nvim_create_augroup("fmt", { clear = true })

vim.api.nvim_create_autocmd("BufWritePre", {
  group = fmt_group,
  pattern = "*",
  callback = function()
    if vim.lsp.buf.format then
      vim.lsp.buf.format()
    else
      -- Fall back to Neoformat
      vim.cmd("Neoformat")
    end
  end,
})
