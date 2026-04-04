local M = {}
local group = vim.api.nvim_create_augroup("config.treesitter", { clear = true })
local warned_missing_cli = false

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
  "rapper",
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

local function register_rapper_parser()
  local parsers = require("nvim-treesitter.parsers")
  local list = parsers.get_parser_configs and parsers.get_parser_configs() or parsers

  list.rapper = {
    install_info = {
      url = "https://github.com/tjdevries/tree-sitter-rapper",
      revision = "99832d42ff758589050c707aea6d6db965240f86",
      files = { "src/parser.c" },
      branch = "main",
    },
    maintainers = { "@derekstride" },
  }
end

local function register_custom_parsers()
  register_reason_parser()
  register_rapper_parser()
end

local function install_parsers_if_available()
  if vim.fn.executable("tree-sitter") == 1 then
    register_custom_parsers()
    require("nvim-treesitter").install(languages)
    return
  end

  if warned_missing_cli then return end
  warned_missing_cli = true

  vim.schedule(function()
    vim.notify("nvim-treesitter parser installation requires the `tree-sitter` CLI on PATH", vim.log.levels.WARN)
  end)
end

M.setup = function()
  require("nvim-treesitter").setup()

  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = "TSUpdate",
    callback = register_custom_parsers,
  })

  register_custom_parsers()
  vim.treesitter.language.register("json", "jsonc")

  vim.api.nvim_create_autocmd("User", {
    group = group,
    pattern = { "LazySync", "LazyInstall", "LazyUpdate" },
    callback = install_parsers_if_available,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = group,
    pattern = "*",
    callback = function(args)
      pcall(vim.treesitter.start, args.buf)
    end,
  })
end

return M
