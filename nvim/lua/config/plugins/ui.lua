return {
  {
    "vim-airline/vim-airline",
    init = function()
      vim.g.airline_highlighting_cache = 1
      vim.g["airline#extensions#tabline#enabled"] = 1
      vim.g["airline#extensions#tabline#buffer_nr_show"] = 1
      vim.g["airline#extensions#tabline#left_sep"] = " "
      vim.g["airline#extensions#tabline#left_alt_sep"] = " "
      vim.g.airline_theme = "taste"

      if vim.g.airline_symbols == nil then
        vim.g.airline_symbols = {
          crypt = "🔒",
          paste = "ρ",
          -- let g:airline_symbols.paste = 'Þ'
          -- let g:airline_symbols.paste = '∥'
          spell = "Ꞩ",
          notexists = "Ɇ",
          whitespace = "Ξ",
          branch = "",
          -- vim.g.airline_symbols.branch = '⎇'
          readonly = "",
          linenr = "☰",
          -- vim.g.airline_symbols.linenr = '␊'
          -- vim.g.airline_symbols.linenr = '␤'
          -- vim.g.airline_symbols.linenr = '¶'

          maxlinenr = "㏑",
          -- let g:airline_symbols.maxlinenr = ''
          -- let g:airline_symbols.maxlinenr = ''
        }
      end

      -- let g:airline_left_sep = '▶'
      -- let g:airline_right_sep = '◀'
      vim.g.airline_left_sep = ""
      vim.g.airline_left_alt_sep = ""
      vim.g.airline_right_sep = ""
      vim.g.airline_right_alt_sep = ""

      -- -- Do not show mode in the command line (handled by Airline)
      vim.opt.showmode = false
      vim.opt.list = true
      vim.opt.listchars = { tab = "▸ ", eol = "¬" }
    end,
  },
  {
    "jordwalke/vim-taste",
    priority = 1000,
    init = function()
      -- color scheme
      vim.opt.termguicolors = true
      vim.opt.background = "dark"
      vim.g.taste_allow_italics = 1
      vim.cmd("colorscheme taste")

      -- Link NormalFloat highlight to Normal
      vim.cmd("highlight def link NormalFloat Normal")

      -- Invisible character settings
      vim.cmd("highlight NonText ctermfg=7 guifg=gray guibg=NONE ctermbg=NONE")
      -- vim.api.nvim_set_hl(0, "NonText", { ctermfg = 7, guifg = "gray" })

      -- Use transparent background if not using GUI (applies to TUI users)
      -- vim.cmd("highlight Normal ctermbg=NONE guibg=NONE")
      -- vim.api.nvim_set_hl(0, "Normal", { ctermbg = "NONE", guibg = "NONE" })

      -- Enable italic comments
      -- TODO(anmonteiro): this is not working, maybe because of `$TERM`
      -- vim.api.nvim_create_autocmd("VimEnter", {
      -- callback = function()
      -- vim.cmd("highlight Comment cterm=italic gui=italic")
      -- end,
      -- })
    end,
  },
  {
    "Yggdroot/indentLine",
    init = function()
      -- Indent guides character
      vim.g.indentLine_char = "│"
    end,
  },
}
