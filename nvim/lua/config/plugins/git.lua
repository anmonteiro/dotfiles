-- git push --set-upstream origin `current_branch`
local function gpsup(...)
  local symbolicRef = vim.fn.system("git symbolic-ref --quiet HEAD"):gsub("\n$", "")
  local currentBranch = symbolicRef:gsub("^refs/heads/", "")
  local args = table.concat({ ... }, " ")
  vim.cmd("Git push --set-upstream origin " .. currentBranch .. " " .. args)
end

return {
  "tpope/vim-fugitive",
  {
    "mhinz/vim-signify",
    config = function()
      vim.cmd("highlight SignColumn ctermbg=NONE guibg=NONE")

      vim.api.nvim_create_user_command("Gpsup", function(opts)
        gpsup(unpack(opts.fargs))
      end, { nargs = "*" })
    end,
    init = function()
      vim.g.signify_vcs_list = { "git" }
      vim.g.signify_realtime = 0
      vim.g.signify_sign_add = "⨁"
      vim.g.signify_sign_change = "✎"
      vim.g.signify_sign_delete = "✖"
    end,
  },
}
