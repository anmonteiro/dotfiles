local M = {}
local group = vim.api.nvim_create_augroup("config.treesitter", { clear = true })

local languages = {
  "c",
  "graphql",
  "go",
  "html",
  "javascript",
  "json",
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
}

local function register_reason_parser()
  local parsers = require("nvim-treesitter.parsers")

  parsers.reason = {
    install_info = {
      path = vim.fn.stdpath("data") .. "/lazy/tree-sitter-reason",
      files = { "src/parser.c", "src/scanner.c" },
      queries = "queries/reason",
    },
  }
end

M.setup = function()
  require("nvim-treesitter").setup()

  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = "TSUpdate",
    callback = register_reason_parser,
  })

  register_reason_parser()
  vim.treesitter.language.register("json", "jsonc")

  if vim.fn.executable("tree-sitter") == 1 then
    require("nvim-treesitter").install(languages)
  else
    vim.schedule(function()
      vim.notify("nvim-treesitter parser installation requires the `tree-sitter` CLI on PATH", vim.log.levels.WARN)
    end)
  end

  vim.api.nvim_create_autocmd("FileType", {
    group = group,
    pattern = "*",
    callback = function(args)
      pcall(vim.treesitter.start, args.buf)
    end,
  })
end

return M
