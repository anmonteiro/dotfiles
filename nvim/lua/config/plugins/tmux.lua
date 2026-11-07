local function navigate(command, direction)
  return function()
    local window = vim.api.nvim_get_current_win()
    vim.cmd(command)

    -- Let Vim move between its splits before handing navigation back to Rex.
    local session = vim.env.REX_SESSION
    if
      vim.api.nvim_get_current_win() ~= window
      or not session
      or session == ""
      or (vim.env.TMUX and vim.env.TMUX ~= "")
      or vim.fn.getcmdwintype() ~= ""
    then
      return
    end

    -- Nix shell initialization can remove Rex's CLI directory from PATH.
    local bin_dir = vim.env.REX_BIN_DIR
    local rex = bin_dir and bin_dir ~= "" and (bin_dir .. "/rex") or "rex"
    if vim.fn.executable(rex) ~= 1 then
      vim.notify_once("Rex navigation: rex executable not found", vim.log.levels.WARN)
      return
    end

    vim.system(
      { rex, "--autostart=false", "focus", "--wrap", "--session", session, direction },
      { text = true, timeout = 2000 },
      vim.schedule_wrap(function(result)
        if result.code ~= 0 then
          vim.notify(
            "Rex navigation failed (exit " .. result.code .. "): " .. vim.trim(result.stderr),
            vim.log.levels.WARN
          )
        end
      end)
    )
  end
end

return {
  "christoomey/vim-tmux-navigator",
  cmd = {
    "TmuxNavigateLeft",
    "TmuxNavigateDown",
    "TmuxNavigateUp",
    "TmuxNavigateRight",
    "TmuxNavigatePrevious",
  },
  keys = {
    { "<M-h>", navigate("TmuxNavigateLeft", "left"), mode = { "n", "t" }, silent = true },
    { "<M-j>", navigate("TmuxNavigateDown", "down"), mode = { "n", "t" }, silent = true },
    { "<M-k>", navigate("TmuxNavigateUp", "up"), mode = { "n", "t" }, silent = true },
    { "<M-l>", navigate("TmuxNavigateRight", "right"), mode = { "n", "t" }, silent = true },
    { "<M-\\>", "<cmd>TmuxNavigatePrevious<cr>", mode = { "n", "t" }, silent = true },
  },
  init = function()
    vim.g.tmux_navigator_no_mappings = true
  end,
}
