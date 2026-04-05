local M = {}
local group = vim.api.nvim_create_augroup("config.treesitter", { clear = true })
local warned_missing_cli = false
local min_tree_sitter_cli = { 0, 26, 1 }

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

local function warn_missing_or_outdated_cli(message)
  if warned_missing_cli then
    return
  end
  warned_missing_cli = true

  vim.schedule(function()
    vim.notify(message, vim.log.levels.WARN)
  end)
end

local function tree_sitter_cli_is_supported()
  if vim.fn.executable("tree-sitter") ~= 1 then
    return false, "nvim-treesitter parser installation requires the `tree-sitter` CLI on PATH"
  end

  local output = vim.trim(vim.fn.system({ "tree-sitter", "--version" }))
  if vim.v.shell_error ~= 0 then
    return false, string.format("Failed to query `tree-sitter --version`: %s", output)
  end

  local version = vim.version.parse(output)
  if version and vim.version.ge(version, min_tree_sitter_cli) then
    return true
  end

  return false,
    string.format(
      "nvim-treesitter on Neovim 0.12 requires tree-sitter-cli >= %d.%d.%d; found %s",
      min_tree_sitter_cli[1],
      min_tree_sitter_cli[2],
      min_tree_sitter_cli[3],
      output
    )
end

local function missing_languages()
  local installed = require("nvim-treesitter").get_installed("parsers")

  return vim.tbl_filter(function(lang)
    return not vim.list_contains(installed, lang)
  end, languages)
end

local function install_missing_parsers_if_available()
  register_custom_parsers()

  local ok, error_message = tree_sitter_cli_is_supported()
  if not ok then
    warn_missing_or_outdated_cli(error_message)
    return false
  end

  local parsers_to_install = missing_languages()
  if #parsers_to_install == 0 then
    return true
  end

  require("nvim-treesitter").install(parsers_to_install)
  return true
end

function M.build()
  register_custom_parsers()

  local ok, error_message = tree_sitter_cli_is_supported()
  if not ok then
    warn_missing_or_outdated_cli(error_message)
    return
  end

  local treesitter = require("nvim-treesitter")
  local parsers_to_install = missing_languages()

  if #parsers_to_install > 0 then
    treesitter.install(parsers_to_install):wait(300000)
  end

  treesitter.update():wait(300000)
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
    callback = install_missing_parsers_if_available,
  })

  vim.api.nvim_create_autocmd("VimEnter", {
    group = group,
    once = true,
    callback = install_missing_parsers_if_available,
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
