local function apply_ui_theme_overrides()
  vim.api.nvim_set_hl(0, "NonText", { fg = "gray", bg = "NONE", ctermfg = 7, ctermbg = "NONE" })
  vim.api.nvim_set_hl(0, "IndentLine", { fg = "gray" })
end

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
    dir = vim.fn.stdpath("config"),
    name = "local-taste",
    lazy = false,
    priority = 1000,
    init = function()
      -- color scheme
      vim.opt.termguicolors = true
      vim.opt.background = "dark"
      vim.g.taste_allow_italics = 1

      local group = vim.api.nvim_create_augroup("local_ui_theme_overrides", { clear = true })
      vim.api.nvim_create_autocmd("ColorScheme", {
        group = group,
        callback = apply_ui_theme_overrides,
      })

      vim.cmd("colorscheme taste")

      require("config.keymap")
    end,
  },
  {
    "nvimdev/indentmini.nvim",
    config = function()
      require("indentmini").setup()
      apply_ui_theme_overrides()
    end,
  },

  {
    "fei6409/log-highlight.nvim",
    config = function()
      require("log-highlight").setup({})
    end,
  },
}
