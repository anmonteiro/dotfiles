local function apply_ui_theme_overrides()
  vim.api.nvim_set_hl(0, "NonText", { fg = "gray", bg = "NONE", ctermfg = 7, ctermbg = "NONE" })
  vim.api.nvim_set_hl(0, "SnacksIndent", { fg = "gray" })
  vim.api.nvim_set_hl(0, "SnacksIndentScope", { link = "Special" })
  vim.api.nvim_set_hl(0, "SnacksIndentChunk", { fg = "gray" })
end

local function lualine_fileformat()
  local encoding = vim.bo.fenc ~= "" and vim.bo.fenc or vim.o.enc
  return string.format("%s[%s]", encoding, vim.bo.fileformat)
end

local function lualine_paste_indicator()
  if not vim.o.paste then return "" end
  return "PASTE"
end

local function lualine_state_symbols()
  local symbols = {}

  if vim.wo.spell then symbols[#symbols + 1] = "Ꞩ" end
  if vim.bo.key ~= "" then symbols[#symbols + 1] = "🔒" end

  local name = vim.api.nvim_buf_get_name(0)
  if name ~= "" and vim.bo.buftype == "" and vim.fn.filereadable(vim.fn.fnamemodify(name, ":p")) == 0 then
    symbols[#symbols + 1] = "Ɇ"
  end

  if #symbols == 0 then return "" end
  return table.concat(symbols, " ")
end

local function lualine_location()
  return string.format("☰ %d/%d ㏑:%d", vim.fn.line("."), vim.fn.line("$"), vim.fn.col("."))
end

local function lualine_options()
  return {
    options = {
      theme = require("config.theme.taste").lualine_theme(),
      globalstatus = false,
      always_divide_middle = true,
      always_show_tabline = true,
      component_separators = { left = "", right = "" },
      section_separators = { left = "", right = "" },
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = {
        {
          "diff",
          symbols = {
            added = "⨁ ",
            modified = "✎ ",
            removed = "✖ ",
          },
        },
      },
      lualine_c = {
        { "branch", icon = "" },
        {
          "filename",
          path = 1,
          symbols = {
            modified = "+",
            readonly = "",
            unnamed = "[No Name]",
            newfile = "[New]",
          },
        },
      },
      lualine_x = {
        {
          lualine_paste_indicator,
          color = function()
            if not vim.o.paste then return nil end
            local ok, hl = pcall(vim.api.nvim_get_hl, 0, { name = "IncSearch", link = false })
            if not ok then return { gui = "bold" } end
            return {
              fg = hl.fg and string.format("#%06x", hl.fg) or nil,
              bg = hl.bg and string.format("#%06x", hl.bg) or nil,
              gui = "bold",
            }
          end,
        },
        lualine_state_symbols,
        { "filetype", icon_only = false },
      },
      lualine_y = {
        lualine_fileformat,
      },
      lualine_z = {
        "progress",
        lualine_location,
      },
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = {
        { "branch", icon = "" },
        {
          "filename",
          path = 1,
          symbols = {
            modified = "+",
            readonly = "",
            unnamed = "[No Name]",
            newfile = "[New]",
          },
        },
      },
      lualine_x = {
        {
          lualine_paste_indicator,
          color = function()
            if not vim.o.paste then return nil end
            local ok, hl = pcall(vim.api.nvim_get_hl, 0, { name = "IncSearch", link = false })
            if not ok then return { gui = "bold" } end
            return {
              fg = hl.fg and string.format("#%06x", hl.fg) or nil,
              bg = hl.bg and string.format("#%06x", hl.bg) or nil,
              gui = "bold",
            }
          end,
        },
        lualine_state_symbols,
        { "filetype", icon_only = false },
      },
      lualine_y = { lualine_fileformat },
      lualine_z = { lualine_location },
    },
    tabline = {
      lualine_a = {
        {
          "buffers",
          cond = function()
            return not vim.bo.filetype:match("^fff_")
          end,
          mode = 4,
          use_mode_colors = true,
          show_filename_only = false,
          hide_filename_extension = false,
          max_length = vim.o.columns * 2 / 3,
          symbols = {
            modified = "+",
            alternate_file = "",
            directory = "",
          },
        },
      },
      lualine_z = {
        function()
          return "buffers"
        end,
      },
    },
    extensions = { "fugitive", "quickfix", "oil" },
  }
end

local function setup_lualine()
  require("lualine").setup(lualine_options())
end

return {
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local group = vim.api.nvim_create_augroup("local_lualine_theme", { clear = true })
      vim.api.nvim_create_autocmd("ColorScheme", {
        group = group,
        callback = setup_lualine,
      })

      setup_lualine()
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
      vim.opt.showmode = false
      vim.opt.list = true
      vim.opt.listchars = { tab = "▸ ", eol = "¬" }

      local group = vim.api.nvim_create_augroup("local_ui_theme_overrides", { clear = true })
      vim.api.nvim_create_autocmd("ColorScheme", {
        group = group,
        callback = apply_ui_theme_overrides,
      })

      vim.cmd("colorscheme taste")
    end,
  },
  {
    "fei6409/log-highlight.nvim",
    config = function()
      require("log-highlight").setup({})
    end,
  },
}
