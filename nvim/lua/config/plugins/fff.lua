return {
  {
    "anmonteiro/fff.nvim",
    branch = "anmonteiro/picker-ui-seam",
    -- build = function()
    -- this will download prebuild binary or try to use existing rustup toolchain to build from source
    -- (if you are using lazy you can use gb for rebuilding a plugin if needed)
    -- require("fff.download").download_or_build_binary()
    -- end,
    -- if you are using nixos
    -- build = "nix run .#release",
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
        "fc",
        function()
          require("fff").live_grep({ query = vim.fn.expand("<cword>") })
        end,
        desc = "Search current word",
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
