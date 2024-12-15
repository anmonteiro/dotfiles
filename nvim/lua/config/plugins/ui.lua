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
    priority = 250,
    init = function()
      -- color scheme
      vim.opt.background = "dark"
      vim.g.taste_allow_italics = 1
      vim.cmd("colorscheme taste")
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
