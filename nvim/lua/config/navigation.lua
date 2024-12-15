-- Set wildmode and wildignore
vim.opt.wildmode = { "longest:list", "full" }
vim.opt.wildignorecase = true
vim.opt.wildignore:append({ "*/_build/*" })
-- vim.opt.wildignore:append("*/_build/*,*/.svn/*,*/node_modules/*")

-- Map `;` to `:`
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })

-- Clear search highlight with `<leader>/`
vim.api.nvim_set_keymap("n", "<leader>/", ":nohlsearch<CR>", { silent = true, noremap = true })

-- Buffer navigation
vim.api.nvim_set_keymap("n", "<leader>n", ":bn<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<leader>p", ":bp<CR>", { silent = true, noremap = true })

-- Split behavior settings
vim.opt.splitbelow = true
vim.opt.splitright = true

-- Link NormalFloat highlight to Normal
vim.cmd("highlight def link NormalFloat Normal")
