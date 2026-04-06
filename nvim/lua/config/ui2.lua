local M = {}

local function set_ui2_enabled(enable, opts)
  opts = opts or {}

  require("vim._core.ui2").enable({
    enable = enable,
    msg = {
      targets = "cmd",
    },
  })

  vim.g.ui2_enabled = enable and 1 or 0

  if not opts.silent then
    vim.notify(string.format("ui2 %s", enable and "enabled" or "disabled"), vim.log.levels.INFO, {
      title = "UI",
    })
  end
end

function M.setup()
  if vim.g.ui2_enabled == nil then
    vim.g.ui2_enabled = 1
  end

  set_ui2_enabled(vim.g.ui2_enabled == 1, { silent = true })

  if vim.g.ui2_commands_ready == 1 then
    return
  end

  vim.g.ui2_commands_ready = 1

  vim.api.nvim_create_user_command("Ui2Enable", function()
    set_ui2_enabled(true)
  end, { desc = "Enable Neovim ui2" })

  vim.api.nvim_create_user_command("Ui2Disable", function()
    set_ui2_enabled(false)
  end, { desc = "Disable Neovim ui2" })

  vim.api.nvim_create_user_command("Ui2Toggle", function()
    set_ui2_enabled(vim.g.ui2_enabled ~= 1)
  end, { desc = "Toggle Neovim ui2" })
end

return M
