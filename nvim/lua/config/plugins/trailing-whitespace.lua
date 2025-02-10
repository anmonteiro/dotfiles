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

return {
  {
    "vim-scripts/ShowTrailingWhitespace",
    config = function()
      vim.cmd("highlight TrailingWhitespaceError ctermbg=red cterm=bold guibg=red")
      vim.cmd("highlight! link ShowTrailingWhitespace TrailingWhitespaceError")

      -- Show trailing whitespace with red highlighting
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = "*",
        callback = strip_trailing_whitespace,
      })
    end,
  },
}
