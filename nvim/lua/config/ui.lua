vim.opt.termguicolors = true

vim.g.did_install_default_menus = 1
vim.g.did_install_syntax_menu = 1

-- -- Line number and column settings
vim.opt.number = true
vim.opt.ruler = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"
vim.opt.cursorline = true

-- -- Invisible character settings
vim.cmd("highlight NonText ctermfg=7 guifg=gray guibg=NONE ctermbg=NONE")
-- vim.api.nvim_set_hl(0, "NonText", { ctermfg = 7, guifg = "gray" })

-- -- Use transparent background if not using GUI (applies to TUI users)
vim.cmd("highlight Normal ctermbg=NONE guibg=NONE")
-- vim.api.nvim_set_hl(0, "Normal", { ctermbg = "NONE", guibg = "NONE" })

-- Enable italic comments
vim.api.nvim_create_autocmd("VimEnter", {
  pattern = "*",
  callback = function()
    vim.cmd("highlight Comment cterm=italic gui=italic")
  end,
})

-- vim-signify settings
vim.g.signify_vcs_list = { "git" }
vim.g.signify_realtime = 0
vim.g.signify_sign_add = "⨁"
vim.g.signify_sign_change = "✎"
vim.g.signify_sign_delete = "✖"
vim.cmd("highlight SignColumn ctermbg=NONE guibg=NONE")

-- Indent guides character
vim.g.indentLine_char = "│"

-- Show trailing whitespace with red highlighting
vim.cmd("highlight TrailingWhitespaceError ctermbg=red cterm=bold guibg=red")
vim.cmd("highlight! link ShowTrailingWhitespace TrailingWhitespaceError")
