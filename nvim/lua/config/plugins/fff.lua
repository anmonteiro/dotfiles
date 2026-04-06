local function get_visual_selection()
  local vmode = vim.fn.mode()
  if not vmode:match("[vV\22]") then
    vmode = vim.fn.visualmode()
  end
  if not vmode:match("[vV\22]") then
    vmode = "v"
  end

  local lines = vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getcurpos(), {
    type = vmode,
  })
  return vim.trim(table.concat(lines, " "):gsub("%s+", " "))
end

local function grep_current_word_or_selection()
  local mode = vim.fn.mode()
  local query = ""

  if mode:match("[vV\22]") then
    query = get_visual_selection()
    local esc = vim.api.nvim_replace_termcodes("<ESC>", true, false, true)
    vim.api.nvim_feedkeys(esc, "nx", false)
  else
    query = vim.fn.expand("<cword>")
  end

  require("fff").live_grep({ query = query })
end

return {
  {
    "dmtrKovalenko/fff.nvim",
    init = function()
      local fff_mcp = vim.fn.exepath("fff-mcp")
      if fff_mcp ~= "" then
        local resolved = vim.uv.fs_realpath(fff_mcp) or fff_mcp
        local path = vim.fn.fnamemodify(resolved, ":h:h")
        if path ~= "" then
          vim.env.CARGO_TARGET_DIR = path
          -- print("fff.nvim CARGO_TARGET_DIR: " .. path)
        end
      end
    end,
    opts = { -- (optional)
      debug = {
        enabled = false, -- we expect your collaboration at least during the beta
        show_scores = true, -- to help us optimize the scoring system, feel free to share your scores!
      },
      hl = {
        normal = "NormalFloat",
        border = "FloatBorder",
        title = "FloatTitle",
        selected = "FFFSelected",
        selected_active = "FFFSelectedActive",
      },
    },
    -- No need to lazy-load with lazy.nvim.
    -- This plugin initializes itself lazily.
    lazy = false,
    keys = {
      {
        "ff", -- try it if you didn't it is a banger keybinding for a picker
        function()
          require("fff").find_files()
        end,
        desc = "FFFind files",
      },
      {
        "fg",
        function()
          require("fff").live_grep()
        end,
        desc = "LiFFFe grep",
      },
      {
        "fz",
        function()
          require("fff").live_grep({
            grep = {
              modes = { "fuzzy", "plain" },
            },
          })
        end,
        desc = "Live fffuzy grep",
      },
      {
        "fw",
        grep_current_word_or_selection,
        mode = { "n", "x" },
        desc = "Search current word or selection",
      },
      {
        "fb",
        function()
          require("config.buffer_picker").open()
        end,
        desc = "FFF buffers",
      },
    },
  },
}
