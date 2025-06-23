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

vim.opt.completeopt = { "menuone", "preview", "noinsert", "noselect" }

vim.opt.mouse = ""

-- Leader key config
-- vim.api.nvim_set_keymap("n", "<SPACE>", "<Nop>", { noremap = true })
-- vim.api.nvim_del_keymap("s", "<SPACE>")
vim.g.mapleader = " "
vim.g.maplocalleader = ","

-- Map `;` to `:`
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })

-- Clear search highlight with `<leader>/`
vim.api.nvim_set_keymap("n", "<leader>/", ":nohlsearch<CR>", { silent = true, noremap = true })

-- Buffer navigation
vim.api.nvim_set_keymap("n", "<leader>n", ":bn<CR>", { silent = true, noremap = true })
vim.api.nvim_set_keymap("n", "<leader>p", ":bp<CR>", { silent = true, noremap = true })

-- From `:help Y`:
--   If you like "Y" to work from the cursor to the end of line (which is more
--   logical, but not Vi-compatible) use ":map Y y$".
vim.api.nvim_set_keymap("n", "Y", "y$", { noremap = true })

-- Check if the buffer is open in another tab / window before switching to it
-- set switchbuf=usetab,useopen

-- Set conceal level to 0 on markdown and json{,c} files
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json", "jsonc", "markdown", "rust" },
  callback = function()
    vim.api.nvim_create_autocmd("BufWinEnter", {
      buffer = vim.api.nvim_get_current_buf(),
      callback = function()
        vim.api.nvim_win_set_option(0, "conceallevel", 0)
      end,
    })
  end,
})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "Vagrantfile",
  callback = function()
    vim.opt_local.filetype = "ruby"
  end,
})

vim.api.nvim_set_keymap("n", "<leader>e", ":Errors<CR>", { silent = true, noremap = true })

-- Make sure `substring` is part of this list. Other items are optional for this completion source
vim.g.completion_matching_strategy_list = { "exact", "substring" }
-- Useful if there's a lot of camel case items
vim.g.completion_matching_ignore_case = 1

local fmt_group = vim.api.nvim_create_augroup("fmt", { clear = true })

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
