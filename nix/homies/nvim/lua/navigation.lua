-- Set wildmode and wildignore
vim.opt.wildmode = { "longest:list", "full" }
vim.opt.wildignore:append({ "*/_build/*", "*/.svn/*", "*/node_modules/*"  })
-- vim.opt.wildignore:append("*/_build/*,*/.svn/*,*/node_modules/*")

-- Map `;` to `:`
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })

-- Clear search highlight with `<leader>/`
vim.api.nvim_set_keymap("n", "<leader>/", ":nohlsearch<CR>", { silent = true, noremap = true })

-- Buffer navigation
vim.api.nvim_set_keymap("n", "<leader>n", ":bn<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<leader>p", ":bp<CR>", { silent = true, noremap = true })

-- Tmux navigation mappings (assuming `tmux-navigator` plugin)
vim.g.tmux_navigator_no_mappings = 1
vim.api.nvim_set_keymap("n", "<M-h>", ":TmuxNavigateLeft<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<M-j>", ":TmuxNavigateDown<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<M-k>", ":TmuxNavigateUp<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<M-l>", ":TmuxNavigateRight<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<M-\\>", ":TmuxNavigatePrevious<CR>", { silent = true, noremap = true })

-- Split behavior settings
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Link NormalFloat highlight to Normal
vim.cmd("highlight def link NormalFloat Normal")

-- git push --set-upstream origin `current_branch`
local function gpsup(...)
  local symbolicRef = vim.fn.system("git symbolic-ref --quiet HEAD"):gsub("\n$", "")
  local currentBranch = symbolicRef:gsub("^refs/heads/", "")
  local args = table.concat({ ... }, " ")
  vim.cmd("Git push --set-upstream origin " .. currentBranch .. " " .. args)
end

vim.api.nvim_create_user_command("Gpsup", function(opts)
  gpsup(unpack(opts.fargs))
end, { nargs = "*" })
