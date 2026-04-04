vim.g.did_install_default_menus = 1
vim.g.did_install_syntax_menu = 1

-- Line number and column settings
vim.opt.number = true
vim.opt.ruler = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"
vim.opt.cursorline = true
vim.opt.hidden = true

-- Set wildmode and wildignore
vim.opt.wildmode = { "longest:list", "full" }
vim.opt.wildignorecase = true
vim.opt.wildignore:append({ "*/_build/*" })
-- vim.opt.wildignore:append("*/_build/*,*/.svn/*,*/node_modules/*")

-- Split behavior settings
vim.opt.splitbelow = true
vim.opt.splitright = true

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

vim.opt.mouse = ""

local map = vim.keymap.set

-- Map `;` to `:`
map("n", ";", ":", { noremap = true })

-- Clear search highlight with `<leader>/`
map("n", "<leader>/", "<cmd>nohlsearch<CR>", { silent = true, noremap = true })

-- Buffer navigation
map("n", "<leader>n", "<cmd>bn<CR>", { silent = true, noremap = true })
map("n", "<leader>p", "<cmd>bp<CR>", { silent = true, noremap = true })

-- From `:help Y`:
--   If you like "Y" to work from the cursor to the end of line (which is more
--   logical, but not Vi-compatible) use ":map Y y$".
map("n", "Y", "y$", { noremap = true })

-- Check if the buffer is open in another tab / window before switching to it
-- set switchbuf=usetab,useopen

vim.api.nvim_create_user_command("Errors", function()
  vim.diagnostic.setloclist({ open = true })
end, { desc = "Open diagnostics in the location list" })

map("n", "<leader>e", "<cmd>Errors<CR>", { silent = true, noremap = true })

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
