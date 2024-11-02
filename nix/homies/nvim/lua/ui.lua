vim.opt.termguicolors = true

vim.g.did_install_default_menus = 1
vim.g.did_install_syntax_menu = 1

-- Disable unneeded built-in plugins to improve startup time
local disabled_builtins = {
  "gzip", "tar", "tarPlugin", "zip", "zipPlugin", "rrhelper",
  "2html_plugin", "vimball", "vimballPlugin", "getscript",
  "getscriptPlugin", "logiPat"
}

for _, plugin in ipairs(disabled_builtins) do
  vim.g["loaded_" .. plugin] = 1
end

-- color scheme
vim.opt.background = "dark"
vim.g.taste_allow_italics = 1
vim.cmd("colorscheme taste")

-- -- Line number and column settings
vim.opt.number = true
vim.opt.ruler = true
vim.opt.signcolumn = "yes"
vim.opt.colorcolumn = "80"
vim.opt.cursorline = true

-- -- Airline settings
vim.g.airline_highlighting_cache = 1
vim.g["airline#extensions#tabline#enabled"] = 1
vim.g["airline#extensions#tabline#buffer_nr_show"] = 1
vim.g["airline#extensions#tabline#left_sep"] = " "
vim.g["airline#extensions#tabline#left_alt_sep"] = " "
vim.g.airline_theme = "taste"


if vim.g.airline_symbols == nil then
  vim.g.airline_symbols = {
    crypt = '🔒',
    paste = 'ρ',
    -- let g:airline_symbols.paste = 'Þ'
    -- let g:airline_symbols.paste = '∥'
    spell = 'Ꞩ',
    notexists = 'Ɇ',
    whitespace = 'Ξ',
    branch = '',
-- vim.g.airline_symbols.branch = '⎇'
    readonly = '',
    linenr = '☰',
-- vim.g.airline_symbols.linenr = '␊'
-- vim.g.airline_symbols.linenr = '␤'
-- vim.g.airline_symbols.linenr = '¶'


    maxlinenr = '㏑',
-- let g:airline_symbols.maxlinenr = ''
-- let g:airline_symbols.maxlinenr = ''

  }
end


-- let g:airline_left_sep = '▶'
-- let g:airline_right_sep = '◀'
vim.g.airline_left_sep = ''
vim.g.airline_left_alt_sep = ''
vim.g.airline_right_sep = ''
vim.g.airline_right_alt_sep = ''


-- -- Do not show mode in the command line (handled by Airline)
vim.opt.showmode = false
vim.opt.list = true
vim.opt.listchars = { tab = "▸ ", eol = "¬" }

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
