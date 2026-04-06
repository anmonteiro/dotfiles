-- git push --set-upstream origin `current_branch`
local function gpsup(opts)
  local result = vim.system({ "git", "symbolic-ref", "--quiet", "--short", "HEAD" }, { text = true }):wait()
  if result.code ~= 0 then
    vim.notify("Gpsup: could not determine the current branch", vim.log.levels.ERROR)
    return
  end

  local current_branch = vim.trim(result.stdout or "")
  if current_branch == "" then
    vim.notify("Gpsup: could not determine the current branch", vim.log.levels.ERROR)
    return
  end

  local args = { "push", "--set-upstream", "origin", current_branch }
  vim.list_extend(args, opts.fargs)
  vim.cmd("Git " .. table.concat(vim.tbl_map(vim.fn.shellescape, args), " "))
end

return {
  {
    "tpope/vim-fugitive",
    config = function()
      vim.api.nvim_create_user_command("Gpsup", function(opts)
        gpsup(opts)
      end, { nargs = "*" })
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    opts = {
      signcolumn = true,
      numhl = false,
      linehl = false,
      current_line_blame = false,
      attach_to_untracked = true,
      update_debounce = 100,
      watch_gitdir = {
        follow_files = true,
      },
    },
    config = function(_, opts)
      require("gitsigns").setup(opts)
    end,
  },
}
