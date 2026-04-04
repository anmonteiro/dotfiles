local detail_highlight_cache = {}
local detail_highlight_cache_size = 0
local max_detail_highlight_cache_size = 500

local function detail_text(ctx)
  return (ctx.item.detail or ""):gsub("\n", " ")
end

local function detail_highlights(ctx)
  local text = detail_text(ctx)
  if text == "" then return "BlinkCmpLabelDescription" end

  local filetype = vim.bo.filetype
  local lang = vim.treesitter.language.get_lang(filetype)
  if not lang then return "BlinkCmpLabelDescription" end

  local cache_key = lang .. "\0" .. text
  local cached = detail_highlight_cache[cache_key]
  if cached then return cached end

  local highlights = {
    { 0, #text, group = "BlinkCmpLabelDescription" },
  }

  local ok, parser = pcall(vim.treesitter.get_string_parser, text, lang)
  if ok then
    parser:parse(true)

    parser:for_each_tree(function(tstree, tree)
      if not tstree then return end

      local query = vim.treesitter.query.get(tree:lang(), "highlights")
      if not query then return end

      for capture, node in query:iter_captures(tstree:root(), text) do
        local _, start_col, _, end_col = node:range()
        local group = "@" .. query.captures[capture] .. "." .. lang
        highlights[#highlights + 1] = {
          start_col,
          end_col,
          group = group,
          priority = 20000,
        }
      end
    end)
  end

  detail_highlight_cache_size = detail_highlight_cache_size + 1
  if detail_highlight_cache_size > max_detail_highlight_cache_size then
    detail_highlight_cache = {}
    detail_highlight_cache_size = 0
  end
  detail_highlight_cache[cache_key] = highlights

  return highlights
end

return {
  {
    "saghen/blink.cmp",
    version = "1.*",
    init = function()
      vim.keymap.set("i", "<C-x><C-o>", function()
        require("blink.cmp").show()
      end, { desc = "Blink completion" })
      vim.keymap.set("i", "<C-x><C-f>", function()
        require("blink.cmp").show({ providers = { "path" } })
      end, { desc = "Blink path completion" })
      vim.keymap.set("i", "<C-x><C-n>", function()
        require("blink.cmp").show({ providers = { "buffer" } })
      end, { desc = "Blink buffer completion" })
    end,
    opts = {
      keymap = {
        preset = "enter",
        ["<C-x><C-o>"] = false,
        ["<C-e>"] = { "hide", "fallback" },
        ["<C-space>"] = false,
        ["<C-n>"] = { "insert_next", "fallback_to_mappings" },
        ["<C-p>"] = { "insert_prev", "fallback_to_mappings" },
      },
      appearance = {
        nerd_font_variant = "mono",
      },
      completion = {
        menu = {
          auto_show = false,
          draw = {
            treesitter = { "lsp" },
            columns = {
              { "kind_icon" },
              { "label", gap = 1 },
              { "detail", gap = 1 },
              { "label_description", gap = 1 },
              { "kind" },
            },
            components = {
              detail = {
                width = { max = 40 },
                text = detail_text,
                highlight = detail_highlights,
              },
            },
          },
        },
        documentation = {
          auto_show = false,
        },
        list = {
          selection = {
            preselect = false,
            auto_insert = false,
          },
        },
      },
      sources = {
        default = { "lsp", "path", "buffer" },
        per_filetype = {
          sql = { inherit_defaults = true, "dadbod" },
          mysql = { inherit_defaults = true, "dadbod" },
          plsql = { inherit_defaults = true, "dadbod" },
        },
        providers = {
          lsp = {
            fallbacks = { "buffer" },
            timeout_ms = function()
              if vim.bo.filetype == "nix" then return 10000 end
              return 2000
            end,
          },
          buffer = {
            score_offset = -3,
          },
          dadbod = {
            name = "Dadbod",
            module = "vim_dadbod_completion.blink",
            fallbacks = { "buffer" },
          },
        },
      },
      fuzzy = {
        implementation = "prefer_rust_with_warning",
      },
    },
  },
}
