vim.opt.hidden = true

-- From `:help Y`:
--   If you like "Y" to work from the cursor to the end of line (which is more
--   logical, but not Vi-compatible) use ":map Y y$".
vim.api.nvim_set_keymap("n", "Y", "y$", { noremap = true })

-- Delete trailing whitespace on save
local function strip_trailing_whitespace()
  local line = vim.fn.line(".")
  local col = vim.fn.col(".")
  local last_search = vim.fn.getreg("/")

  vim.cmd("%s/\\s\\+$//e")

  -- Delete the last entry from the search history, which is the substitution
  -- command
  vim.fn.histdel("search", -1)
  vim.fn.setreg("/", last_search)
  vim.fn.cursor(line, col)
end
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*",
  callback = strip_trailing_whitespace,
})

-- Share system clipboard
vim.opt.clipboard:append({ "unnamed", "unnamedplus" })

-- Disable swap files
vim.opt.swapfile = false

vim.opt.updatetime = 200

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.autoindent = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.completeopt = { "menuone", "preview", "noinsert", "noselect" }

vim.opt.mouse = ""

-- Check if the buffer is open in another tab / window before switching to it
-- set switchbuf=usetab,useopen

-- Set conceal level to 0 on markdown and json{,c} files
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json", "jsonc", "markdown" },
  callback = function()
    vim.api.nvim_create_autocmd("BufWinEnter", {
      buffer = vim.api.nvim_get_current_buf(),
      callback = function()
        vim.api.nvim_win_set_option(0, "conceallevel", 0)
      end,
    })
  end,
})

-- Leader key config
-- vim.api.nvim_set_keymap("n", "<SPACE>", "<Nop>", { noremap = true })
-- vim.api.nvim_del_keymap("s", "<SPACE>")
vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "Vagrantfile",
  callback = function()
    vim.opt_local.filetype = "ruby"
  end,
})

vim.opt.statusline = vim.opt.statusline + "%#warningmsg#" + "%{SyntasticStatuslineFlag()}" + "%*"
vim.g.syntastic_always_populate_loc_list = 1
vim.g.syntastic_aggregate_errors = 1
vim.g.syntastic_check_on_open = 1
vim.g.syntastic_check_on_wq = 0
vim.g.syntastic_error_symbol = "✗"
vim.g.syntastic_warning_symbol = "⚠"
vim.cmd("highlight link SyntasticWarningSign Typedef")

vim.api.nvim_set_keymap("n", "<leader>e", ":Errors<CR>", { silent = true, noremap = true })

-- Rainbow Parens
vim.g.rainbow_active = 1

vim.g.neoformat_javascript_prettier = {
  exe = "./node_modules/.bin/prettier",
  args = { "--single-quote", "--stdin", "--stdin-filepath", "%:p" },
  stdin = 1,
}
vim.g.neoformat_typescript_prettier = vim.g.neoformat_javascript_prettier
vim.g.neoformat_typescriptreact_prettier = vim.g.neoformat_javascript_prettier
vim.g.neoformat_css_prettier = vim.g.neoformat_javascript_prettier
vim.g.neoformat_scss_prettier = vim.g.neoformat_javascript_prettier

vim.g.neoformat_sql_pg_format = {
  exe = "pg_format",
  args = { "-B", "-s", "2", "-w", "80", "-" },
  stdin = 1,
}

vim.api.nvim_create_autocmd("FileType", {
  pattern = "sql",
  command = [[setlocal omnifunc=vim_dadbod_completion#omni]],
})
-- Source is automatically added, you just need to include it in the chain complete list
vim.g.completion_chain_complete_list = {
  sql = {
    { complete_items = { "vim-dadbod-completion" } },
  },
}
-- Make sure `substring` is part of this list. Other items are optional for this completion source
vim.g.completion_matching_strategy_list = { "exact", "substring" }
-- Useful if there's a lot of camel case items
vim.g.completion_matching_ignore_case = 1

require("config.clojure")

require("config.reason-ocaml")

local fmt_group = vim.api.nvim_create_augroup("fmt", { clear = true })

vim.api.nvim_create_autocmd("BufWritePre", {
  group = fmt_group,
  pattern = "*",
  callback = function()
    local client = vim.lsp.get_active_clients({ bufnr = 0 })[1]

    if client and client.supports_method("textDocument/formatting") then
      vim.lsp.buf.format()
    else
      -- Fall back to Neoformat
      vim.cmd("Neoformat")
    end
  end,
})

-- Snoopy Mode
-- http://vim.wikia.com/wiki/Invert_the_number_row_keys_for_faster_typing
-- map each number to its shift-key character
-- inoremap 1 !
-- inoremap 2 @
-- inoremap 3 #
-- inoremap 4 $
-- inoremap 5 %
-- inoremap 6 ^
-- inoremap 7 &
-- inoremap 8 *
-- inoremap 9 (
-- inoremap 0 )
-- "inoremap - _
-- " and then the opposite
-- inoremap ! 1
-- inoremap @ 2
-- inoremap # 3
-- inoremap $ 4
-- inoremap % 5
-- inoremap ^ 6
-- inoremap & 7
-- inoremap * 8
-- inoremap ( 9
-- inoremap ) 0
-- inoremap _ -
