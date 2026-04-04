local function normal_count_map(callback)
  return function()
    if vim.v.count > 0 then
      callback.count(vim.v.count1)
    else
      callback.current()
    end
  end
end

local function visual_map(callback)
  local esc = vim.api.nvim_replace_termcodes("<ESC>", true, false, true)

  return function()
    vim.api.nvim_feedkeys(esc, "nx", false)
    callback(vim.fn.visualmode())
  end
end

local function setup_editing_autocmds()
  vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    pattern = "Vagrantfile",
    callback = function()
      vim.bo.filetype = "ruby"
    end,
  })

  -- Window-local display overrides.
  -- Keep this disabled unless conceal in Markdown/JSON becomes annoying again.
  -- The nested BufWinEnter was intentional because conceallevel is window-local.
  --[[
  vim.api.nvim_create_autocmd("FileType", {
    pattern = { "json", "jsonc", "markdown" },
    callback = function()
      vim.api.nvim_create_autocmd("BufWinEnter", {
        buffer = vim.api.nvim_get_current_buf(),
        callback = function()
          vim.wo.conceallevel = 0
        end,
      })
    end,
  })
  ]]
end

return {
  {
    "kylechui/nvim-surround",
    branch = "main",
    init = setup_editing_autocmds,
    config = function()
      require("nvim-surround").setup({})
    end,
  },
  {
    "numToStr/Comment.nvim",
    config = function()
      local api = require("Comment.api")
      local ft = require("Comment.ft")

      ft.set("reason", { "//%s", "/*%s*/" })

      require("Comment").setup({
        padding = true,
        sticky = true,
        mappings = {
          basic = false,
          extra = false,
        },
      })

      vim.keymap.set("n", "<leader>cc", normal_count_map(api.comment.linewise), {
        desc = "Comment line",
        noremap = true,
        silent = true,
      })
      vim.keymap.set("x", "<leader>cc", visual_map(api.comment.linewise), {
        desc = "Comment selection",
        noremap = true,
        silent = true,
      })

      vim.keymap.set("n", "<leader>c ", normal_count_map(api.toggle.linewise), {
        desc = "Toggle comment",
        noremap = true,
        silent = true,
      })
      vim.keymap.set("x", "<leader>c ", visual_map(api.toggle.linewise), {
        desc = "Toggle comment",
        noremap = true,
        silent = true,
      })

      vim.keymap.set("n", "<leader>cu", normal_count_map(api.uncomment.linewise), {
        desc = "Uncomment line",
        noremap = true,
        silent = true,
      })
      vim.keymap.set("x", "<leader>cu", visual_map(api.uncomment.linewise), {
        desc = "Uncomment selection",
        noremap = true,
        silent = true,
      })

      vim.keymap.set("n", "<leader>cm", normal_count_map(api.toggle.blockwise), {
        desc = "Comment block",
        noremap = true,
        silent = true,
      })
      vim.keymap.set("x", "<leader>cm", visual_map(api.toggle.blockwise), {
        desc = "Comment block",
        noremap = true,
        silent = true,
      })

      vim.keymap.set("n", "<leader>cA", api.locked("insert.linewise.eol"), {
        desc = "Append comment",
        noremap = true,
        silent = true,
      })
    end,
  },
  {
    "HiPhish/rainbow-delimiters.nvim",
    config = function()
      require("rainbow-delimiters.setup").setup({
        highlight = {
          "RainbowDelimiterRed",
          "RainbowDelimiterYellow",
          "RainbowDelimiterBlue",
          "RainbowDelimiterOrange",
          "RainbowDelimiterGreen",
          "RainbowDelimiterViolet",
          "RainbowDelimiterCyan",
        },
      })
    end,
  },
}
