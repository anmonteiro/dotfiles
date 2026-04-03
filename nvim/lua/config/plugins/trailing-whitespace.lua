local function is_normal_file_window(win)
  if not win or not vim.api.nvim_win_is_valid(win) then
    return false
  end

  local win_config = vim.api.nvim_win_get_config(win)
  if win_config.relative ~= "" then
    return false
  end

  local buf = vim.api.nvim_win_get_buf(win)
  local bo = vim.bo[buf]
  if bo.buftype ~= "" and bo.buftype ~= "acwrite" then
    return false
  end

  return bo.modifiable and not bo.binary
end

local function clear_match(win)
  local ok, match_id = pcall(vim.api.nvim_win_get_var, win, "trailing_whitespace_match")
  if ok and match_id then
    pcall(vim.fn.matchdelete, match_id, win)
    pcall(vim.api.nvim_win_del_var, win, "trailing_whitespace_match")
  end
end

local function update_match(win, is_insert_mode)
  if not is_normal_file_window(win) then
    clear_match(win)
    return
  end

  local pattern = is_insert_mode and [[\s\+\%#\@<!$]] or [[\s\+$]]
  clear_match(win)

  local match_id = vim.fn.matchadd("ShowTrailingWhitespace", pattern, -1, -1, { window = win })
  pcall(vim.api.nvim_win_set_var, win, "trailing_whitespace_match", match_id)
end

local function update_current_window(is_insert_mode)
  update_match(vim.api.nvim_get_current_win(), is_insert_mode)
end

local function set_highlights()
  vim.api.nvim_set_hl(0, "TrailingWhitespaceError", {
    ctermbg = "red",
    cterm = { bold = true },
    bg = "red",
  })
  vim.api.nvim_set_hl(0, "ShowTrailingWhitespace", { link = "TrailingWhitespaceError" })
end

return {
  {
    dir = vim.fn.stdpath("config"),
    name = "local-trailing-whitespace",
    lazy = false,
    config = function()
      set_highlights()

      local group = vim.api.nvim_create_augroup("LocalTrailingWhitespace", { clear = true })

      vim.api.nvim_create_autocmd({ "BufWinEnter", "WinEnter", "InsertLeave" }, {
        group = group,
        callback = function()
          update_current_window(false)
        end,
      })

      vim.api.nvim_create_autocmd("InsertEnter", {
        group = group,
        callback = function()
          update_current_window(true)
        end,
      })

      vim.api.nvim_create_autocmd("ColorScheme", {
        group = group,
        callback = set_highlights,
      })
    end,
  },
}
