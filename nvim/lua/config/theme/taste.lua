local definitions = require("config.theme.taste_definitions")

local M = {}

local colors_bank = {
  strawberry = { "ff8c82", "ed5353", "c6262e", "a10705", "7a0000" },
  orange = { "ffc27d", "ffa154", "f37329", "cc3b02", "a62100" },
  banana = { "fff394", "ffe16b", "f9c440", "d48e15", "ad5f00" },
  lime = { "d1ff82", "9bdb4d", "68b723", "3a9104", "206b00" },
  blueberry = { "8cd5ff", "64baff", "3689e6", "0d52bf", "002e99" },
  grape = { "e4c6fa", "cd9ef7", "a56de2", "7239b3", "452981" },
  cocoa = { "a3907c", "8a715e", "715344", "57392d", "3d211b" },
  silver = { "fafafa", "d4d4d4", "abacae", "7e8087", "555761" },
  slate = { "95a3ab", "667885", "485a6c", "273445", "0e141f" },
  black = { "666666", "4d4d4d", "333333", "1a1a1a", "000000" },
}

local function clamp(value, min, max)
  return math.min(math.max(value, min), max)
end

local function normalize_hex(color)
  return color:gsub("^#", "")
end

local function to_hex(color)
  return "#" .. normalize_hex(color)
end

local function blend(from, to, pct)
  local source = normalize_hex(from)
  local target = normalize_hex(to)
  local blended = {}

  for index = 1, 6, 2 do
    local source_value = tonumber(source:sub(index, index + 1), 16)
    local target_value = tonumber(target:sub(index, index + 1), 16)
    local value = math.floor(source_value + ((target_value - source_value) * pct) / 100)
    blended[#blended + 1] = string.format("%02x", value)
  end

  return table.concat(blended)
end

local function colors_for(brightness)
  local level = clamp(brightness, 0, 4)
  local colors = {}

  for name, shades in pairs(colors_bank) do
    colors[name] = shades[5 - level]
  end

  return colors
end

local function brightness_from_global(name, default)
  local value = tonumber(vim.g[name]) or default

  if value < 1 or value > 3 then
    vim.notify(
      string.format("taste colorscheme configured with %s outside valid range 1-3. saw %s", name, value),
      vim.log.levels.WARN
    )
  end

  return clamp(value, 1, 3)
end

local function derived_colors(background)
  local dark_brightness = brightness_from_global("taste_dark_brightness", 3)
  local light_brightness = brightness_from_global("taste_light_brightness", 3)
  local palette = {}
  local colors

  if background == "light" then
    local lighter = colors_for(light_brightness + 1)
    colors = colors_for(light_brightness)
    local darker = colors_for(light_brightness - 1)
    local darkerer = colors_for(light_brightness - 2)

    palette.cyan = blend(darkerer.blueberry, darker.blueberry, 0)
    palette.blue = blend(darkerer.blueberry, darker.blueberry, 100)
    palette.purple = blend(darkerer.grape, darker.grape, 0)
    palette.green = blend(darkerer.lime, darker.lime, 0)
    palette.red = blend(darkerer.strawberry, darker.strawberry, 100)
    palette.red2 = blend(darker.strawberry, colors.strawberry, 100)
    palette.orange = blend(darkerer.banana, darker.banana, 0)
    palette.orange2 = blend(darkerer.banana, darker.banana, 0)

    palette.syntax_bg = blend(colors.silver, lighter.silver, 80)
    palette.fg = lighter.black
    palette.fg_dim = darker.silver
    palette.syntax_fg = palette.fg
    palette.syntax_fg_dim = palette.fg_dim
    palette.syntax_fold_bg = palette.syntax_fg_dim

    palette.chrome_bg = blend(colors.silver, lighter.silver, 50)
    palette.chrome_fg = palette.fg
    palette.chrome_fg_dim = palette.fg_dim
    palette.gutter_bg = palette.chrome_bg
    palette.gutter_fg = palette.fg_dim
    palette.gutter_fg_dim = palette.fg_dim
    palette.vertsplit_bg = palette.chrome_bg

    palette.modal_bg = blend(colors.silver, lighter.silver, 30)
    palette.modal_button_bg = blend(darker.silver, colors.silver, 0)
    palette.syntax_cursorline = palette.modal_bg

    palette.syntax_accent = darker.blueberry
    palette.special_grey = "ffffff"
    palette.visual = blend(palette.modal_bg, darker.grape, 8)
    palette.diff_add_fg = blend(palette.syntax_bg, palette.green, 20)
    palette.diff_add_bg = palette.green
    palette.diff_delete_fg = palette.red
    palette.diff_delete_bg = blend(palette.syntax_bg, palette.diff_delete_fg, 20)
    palette.search2 = darker.banana
    palette.search = colors.banana
    palette.search2_bg = blend(palette.syntax_fg, palette.search2, 15)
    palette.search_bg = blend(palette.syntax_fg, palette.search, 5)
  else
    local lighter = colors_for(dark_brightness + 1)
    colors = colors_for(dark_brightness)
    local darker = colors_for(dark_brightness - 1)
    local darkerer = colors_for(dark_brightness - 2)

    palette.cyan = blend(colors.blueberry, lighter.blueberry, 100)
    palette.blue = blend(colors.blueberry, lighter.blueberry, 0)
    palette.purple = blend(colors.grape, lighter.grape, 0)
    palette.green = blend(colors.lime, lighter.lime, 100)
    palette.red = blend(colors.strawberry, lighter.strawberry, 100)
    palette.red2 = blend(colors.strawberry, lighter.strawberry, 0)
    palette.orange = blend(darker.orange, colors.orange, 100)
    palette.orange2 = blend(darker.orange, lighter.orange, 100)

    palette.syntax_bg = blend(darker.black, colors.black, 20)
    palette.fg = colors.silver
    palette.fg_dim = lighter.black
    palette.syntax_fg = palette.fg
    palette.syntax_fg_dim = palette.fg_dim
    palette.syntax_fold_bg = palette.syntax_fg_dim

    palette.chrome_bg = blend(darker.black, colors.black, 50)
    palette.chrome_fg = palette.fg
    palette.chrome_fg_dim = palette.fg_dim
    palette.gutter_bg = palette.chrome_bg
    palette.gutter_fg = palette.fg_dim
    palette.gutter_fg_dim = palette.fg_dim
    palette.vertsplit_bg = palette.chrome_bg

    palette.modal_bg = blend(darker.black, colors.black, 80)
    palette.modal_button_bg = blend(colors.black, lighter.black, 0)
    palette.syntax_cursorline = palette.modal_bg

    palette.syntax_accent = lighter.blueberry
    palette.special_grey = "ffffff"
    palette.visual = blend(palette.modal_bg, darker.grape, 8)
    palette.diff_add_fg = blend(palette.syntax_bg, palette.green, 20)
    palette.diff_add_bg = palette.green
    palette.diff_delete_fg = palette.red
    palette.diff_delete_bg = blend(palette.syntax_bg, palette.diff_delete_fg, 20)
    palette.search2 = colors.banana
    palette.search = lighter.banana
    palette.search2_bg = blend(palette.syntax_bg, palette.search2, 15)
    palette.search_bg = blend(palette.syntax_bg, palette.search, 5)
  end

  local terminal_ansi_colors = {
    to_hex(palette.syntax_bg),
    to_hex(palette.red),
    to_hex(palette.green),
    to_hex(palette.search),
    to_hex(palette.blue),
    to_hex(palette.purple),
    to_hex(palette.cyan),
    to_hex(palette.syntax_fg),
    to_hex(palette.chrome_bg),
    to_hex(palette.red2),
    to_hex(colors.lime),
    to_hex(palette.search2),
    to_hex(colors.blueberry),
    to_hex(colors.grape),
    to_hex(colors.blueberry),
    to_hex(palette.syntax_fg_dim),
  }

  return palette, terminal_ansi_colors
end

local function resolve_color(color, palette)
  if color == nil then
    return nil
  end

  return to_hex(palette[color] or color)
end

local function apply_attributes(hl, attr)
  if attr == nil or attr == "" or attr == "none" then
    return
  end

  local allow_italics = vim.g.taste_allow_italics == 1

  for token in attr:gmatch("[^,]+") do
    local style = vim.trim(token)

    if style == "italic" and not allow_italics then
      goto continue
    end

    hl[style] = true

    ::continue::
  end
end

local function apply_extra_highlights(palette)
  local visual_bg = to_hex(blend(palette.syntax_bg, palette.blue, 18))
  local active_visual_bg = to_hex(blend(palette.syntax_bg, palette.blue, 28))
  local float_bg = to_hex(blend(palette.syntax_bg, palette.modal_bg, 20))
  local float_border_fg = to_hex(blend(palette.chrome_fg_dim, palette.syntax_bg, 40))
  local soft_dim_fg = to_hex(blend(palette.syntax_fg_dim, palette.syntax_fg, 18))
  local title_fg = resolve_color("syntax_fg", palette)
  local active_fg = resolve_color("syntax_fg", palette)
  local ignored_fg = resolve_color("fg_dim", palette)
  local normal_nc_bg = to_hex(blend(palette.syntax_bg, palette.chrome_bg, 35))
  local inlay_bg = to_hex(blend(palette.syntax_bg, palette.modal_bg, 35))
  local match_bg = to_hex(blend(palette.syntax_bg, palette.blue, 12))
  local added_bg = to_hex(blend(palette.syntax_bg, palette.green, 10))
  local changed_bg = to_hex(blend(palette.syntax_bg, palette.orange, 10))
  local removed_bg = to_hex(blend(palette.syntax_bg, palette.red, 10))
  local shadow_bg = to_hex(blend(palette.syntax_bg, palette.chrome_bg, 55))

  vim.api.nvim_set_hl(0, "NormalFloat", { fg = resolve_color("syntax_fg", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "FloatBorder", { fg = float_border_fg, bg = float_bg })
  vim.api.nvim_set_hl(0, "FloatTitle", { fg = title_fg, bg = float_bg, bold = true })
  vim.api.nvim_set_hl(0, "FloatFooter", { fg = resolve_color("chrome_fg_dim", palette), bg = float_bg, bold = true })
  vim.api.nvim_set_hl(0, "FloatShadow", { bg = shadow_bg, blend = 80 })
  vim.api.nvim_set_hl(0, "FloatShadowThrough", { bg = shadow_bg, blend = 100 })
  vim.api.nvim_set_hl(0, "NormalNC", { fg = resolve_color("syntax_fg_dim", palette), bg = normal_nc_bg })
  vim.api.nvim_set_hl(
    0,
    "WinSeparator",
    { fg = resolve_color("vertsplit_bg", palette), bg = resolve_color("chrome_bg", palette) }
  )
  vim.api.nvim_set_hl(0, "WinBar", { fg = resolve_color("chrome_fg", palette), bg = resolve_color("chrome_bg", palette), bold = true })
  vim.api.nvim_set_hl(0, "WinBarNC", { fg = resolve_color("chrome_fg_dim", palette), bg = resolve_color("chrome_bg", palette) })
  vim.api.nvim_set_hl(0, "MsgArea", { fg = resolve_color("syntax_fg", palette), bg = resolve_color("chrome_bg", palette) })
  vim.api.nvim_set_hl(0, "MsgSeparator", { fg = float_border_fg, bg = resolve_color("chrome_bg", palette) })
  vim.api.nvim_set_hl(0, "LineNrAbove", { fg = resolve_color("gutter_fg", palette), bg = resolve_color("gutter_bg", palette) })
  vim.api.nvim_set_hl(0, "LineNrBelow", { fg = resolve_color("gutter_fg", palette), bg = resolve_color("gutter_bg", palette) })
  vim.api.nvim_set_hl(0, "SignColumn", { fg = resolve_color("gutter_fg_dim", palette), bg = resolve_color("gutter_bg", palette) })
  vim.api.nvim_set_hl(0, "Whitespace", { fg = resolve_color("gutter_fg_dim", palette) })
  vim.api.nvim_set_hl(0, "EndOfBuffer", { fg = resolve_color("syntax_bg", palette), bg = resolve_color("syntax_bg", palette) })
  vim.api.nvim_set_hl(0, "Comment", { fg = soft_dim_fg, italic = true })
  vim.api.nvim_set_hl(0, "CursorLineSign", { fg = resolve_color("gutter_fg", palette), bg = resolve_color("syntax_cursorline", palette) })
  vim.api.nvim_set_hl(0, "CursorLineFold", { fg = resolve_color("gutter_fg", palette), bg = resolve_color("syntax_cursorline", palette) })
  vim.api.nvim_set_hl(0, "TermCursor", { fg = resolve_color("syntax_bg", palette), bg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "TermCursorNC", { fg = resolve_color("syntax_bg", palette), bg = resolve_color("syntax_fg_dim", palette) })

  vim.api.nvim_set_hl(0, "CurSearch", { fg = resolve_color("syntax_bg", palette), bg = resolve_color("search2", palette), bold = true })
  vim.api.nvim_set_hl(0, "Substitute", { fg = resolve_color("syntax_bg", palette), bg = resolve_color("orange", palette), bold = true })
  vim.api.nvim_set_hl(0, "QuickFixLine", { fg = resolve_color("syntax_fg", palette), bg = active_visual_bg, bold = true })

  vim.api.nvim_set_hl(0, "PmenuKind", { fg = resolve_color("syntax_fg_dim", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(
    0,
    "PmenuKindSel",
    { fg = resolve_color("syntax_bg", palette), bg = resolve_color("blue", palette), bold = true }
  )
  vim.api.nvim_set_hl(0, "PmenuExtra", { fg = resolve_color("syntax_fg", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(
    0,
    "PmenuExtraSel",
    { fg = resolve_color("syntax_bg", palette), bg = resolve_color("blue", palette) }
  )
  vim.api.nvim_set_hl(0, "PmenuMatch", { fg = resolve_color("blue", palette), bg = resolve_color("modal_bg", palette), bold = true })
  vim.api.nvim_set_hl(
    0,
    "PmenuMatchSel",
    { fg = resolve_color("search2", palette), bg = resolve_color("blue", palette), bold = true }
  )
  vim.api.nvim_set_hl(0, "SnippetTabstop", { fg = resolve_color("syntax_fg", palette), bg = match_bg, underline = true })

  vim.api.nvim_set_hl(0, "BlinkCmpMenu", { fg = resolve_color("syntax_fg", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(0, "BlinkCmpMenuBorder", { fg = float_border_fg, bg = float_bg })
  vim.api.nvim_set_hl(0, "BlinkCmpDoc", { fg = resolve_color("syntax_fg", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "BlinkCmpDocBorder", { fg = float_border_fg, bg = float_bg })
  vim.api.nvim_set_hl(0, "BlinkCmpLabel", { fg = resolve_color("syntax_fg", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(0, "BlinkCmpLabelMatch", { fg = resolve_color("blue", palette), bg = resolve_color("modal_bg", palette), bold = true })
  vim.api.nvim_set_hl(0, "BlinkCmpLabelDescription", { fg = resolve_color("syntax_fg_dim", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(0, "BlinkCmpKind", { fg = resolve_color("chrome_fg_dim", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(0, "BlinkCmpSource", { fg = resolve_color("gutter_fg", palette), bg = resolve_color("modal_bg", palette) })
  vim.api.nvim_set_hl(0, "BlinkCmpGhostText", { fg = resolve_color("syntax_fg_dim", palette), italic = true })

  vim.api.nvim_set_hl(0, "Added", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "Changed", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "Removed", { fg = resolve_color("red", palette) })

  vim.api.nvim_set_hl(0, "DiagnosticError", { fg = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticWarn", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticInfo", { fg = resolve_color("blue", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticHint", { fg = resolve_color("cyan", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticOk", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticSignError", { fg = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticSignWarn", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticSignInfo", { fg = resolve_color("blue", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticSignHint", { fg = resolve_color("cyan", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticSignOk", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticFloatingError", { fg = resolve_color("red", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "DiagnosticFloatingWarn", { fg = resolve_color("orange", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "DiagnosticFloatingInfo", { fg = resolve_color("blue", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "DiagnosticFloatingHint", { fg = resolve_color("cyan", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "DiagnosticFloatingOk", { fg = resolve_color("green", palette), bg = float_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualTextError", { fg = resolve_color("red", palette), bg = removed_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualTextWarn", { fg = resolve_color("orange", palette), bg = changed_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualTextInfo", { fg = resolve_color("blue", palette), bg = match_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualTextHint", { fg = resolve_color("cyan", palette), bg = match_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualTextOk", { fg = resolve_color("green", palette), bg = added_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualLinesError", { fg = resolve_color("red", palette), bg = removed_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualLinesWarn", { fg = resolve_color("orange", palette), bg = changed_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualLinesInfo", { fg = resolve_color("blue", palette), bg = match_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualLinesHint", { fg = resolve_color("cyan", palette), bg = match_bg })
  vim.api.nvim_set_hl(0, "DiagnosticVirtualLinesOk", { fg = resolve_color("green", palette), bg = added_bg })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { undercurl = true, sp = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = resolve_color("blue", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = resolve_color("cyan", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineOk", { undercurl = true, sp = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticDeprecated", { strikethrough = true, sp = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "DiagnosticUnnecessary", { fg = resolve_color("syntax_fg_dim", palette), italic = true })

  vim.api.nvim_set_hl(0, "LspReferenceText", { bg = match_bg })
  vim.api.nvim_set_hl(0, "LspReferenceRead", { bg = match_bg })
  vim.api.nvim_set_hl(0, "LspReferenceWrite", { bg = active_visual_bg, underline = true })
  vim.api.nvim_set_hl(0, "LspCodeLens", { fg = resolve_color("syntax_fg_dim", palette), italic = true })
  vim.api.nvim_set_hl(0, "LspCodeLensSeparator", { fg = resolve_color("chrome_fg_dim", palette) })
  vim.api.nvim_set_hl(0, "LspInlayHint", { fg = resolve_color("chrome_fg_dim", palette), bg = inlay_bg, italic = true })
  vim.api.nvim_set_hl(0, "LspInfoBorder", { fg = float_border_fg, bg = float_bg })

  vim.api.nvim_set_hl(0, "FFFSelected", { fg = resolve_color("syntax_fg", palette), bg = visual_bg })
  vim.api.nvim_set_hl(0, "FFFSelectedActive", { fg = active_fg, bg = active_visual_bg, bold = true })

  vim.api.nvim_set_hl(0, "FFFGitStaged", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "FFFGitModified", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "FFFGitDeleted", { fg = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "FFFGitRenamed", { fg = resolve_color("purple", palette) })
  vim.api.nvim_set_hl(0, "FFFGitUntracked", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "FFFGitIgnored", { fg = ignored_fg })

  vim.api.nvim_set_hl(0, "FFFGitSignStaged", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "FFFGitSignModified", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "FFFGitSignDeleted", { fg = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "FFFGitSignRenamed", { fg = resolve_color("purple", palette) })
  vim.api.nvim_set_hl(0, "FFFGitSignUntracked", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "FFFGitSignIgnored", { fg = ignored_fg })

  vim.api.nvim_set_hl(0, "FFFGitSignStagedSelected", { fg = resolve_color("green", palette), bg = active_visual_bg })
  vim.api.nvim_set_hl(0, "FFFGitSignModifiedSelected", { fg = resolve_color("orange", palette), bg = active_visual_bg })
  vim.api.nvim_set_hl(0, "FFFGitSignDeletedSelected", { fg = resolve_color("red", palette), bg = active_visual_bg })
  vim.api.nvim_set_hl(0, "FFFGitSignRenamedSelected", { fg = resolve_color("purple", palette), bg = active_visual_bg })
  vim.api.nvim_set_hl(0, "FFFGitSignUntrackedSelected", { fg = resolve_color("green", palette), bg = active_visual_bg })
  vim.api.nvim_set_hl(0, "FFFGitSignIgnoredSelected", { fg = ignored_fg, bg = active_visual_bg })

  vim.api.nvim_set_hl(0, "RainbowDelimiterRed", { fg = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "RainbowDelimiterYellow", { fg = resolve_color("search", palette) })
  vim.api.nvim_set_hl(0, "RainbowDelimiterBlue", { fg = resolve_color("blue", palette) })
  vim.api.nvim_set_hl(0, "RainbowDelimiterOrange", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "RainbowDelimiterGreen", { fg = resolve_color("green", palette) })
  vim.api.nvim_set_hl(0, "RainbowDelimiterViolet", { fg = resolve_color("purple", palette) })
  vim.api.nvim_set_hl(0, "RainbowDelimiterCyan", { fg = resolve_color("cyan", palette) })
end

local function apply_treesitter_highlights(palette)
  local separator_fg = to_hex(blend(palette.syntax_fg_dim, palette.syntax_fg, 58))
  local special_separator_fg = to_hex(blend(palette.orange2, palette.syntax_fg, 28))

  local treesitter_links = {
    ["@comment"] = "Comment",
    ["@comment.documentation"] = "Comment",
    ["@comment.error"] = "DiagnosticError",
    ["@comment.warning"] = "DiagnosticWarn",
    ["@comment.todo"] = "Todo",
    ["@comment.note"] = "DiagnosticInfo",
    ["@constant"] = "Constant",
    ["@constant.builtin"] = "Special",
    ["@constant.macro"] = "Macro",
    ["@string"] = "String",
    ["@string.escape"] = "SpecialChar",
    ["@string.regexp"] = "String",
    ["@string.special"] = "SpecialChar",
    ["@character"] = "Character",
    ["@character.special"] = "SpecialChar",
    ["@number"] = "Number",
    ["@number.float"] = "Float",
    ["@boolean"] = "Boolean",
    ["@constructor"] = "Type",
    ["@operator"] = "Operator",
    ["@keyword"] = "Keyword",
    ["@keyword.function"] = "Keyword",
    ["@keyword.operator"] = "Operator",
    ["@keyword.return"] = "Keyword",
    ["@keyword.repeat"] = "Repeat",
    ["@keyword.conditional"] = "Conditional",
    ["@keyword.exception"] = "Exception",
    ["@keyword.import"] = "Include",
    ["@keyword.directive"] = "PreProc",
    ["@function"] = "Function",
    ["@function.builtin"] = "Special",
    ["@function.call"] = "Function",
    ["@function.method"] = "Function",
    ["@function.method.call"] = "Function",
    ["@label"] = "Label",
    ["@type"] = "Type",
    ["@type.builtin"] = "Type",
    ["@type.definition"] = "Typedef",
    ["@type.qualifier"] = "StorageClass",
    ["@attribute"] = "Macro",
    ["@attribute.builtin"] = "Special",
    ["@tag"] = "Tag",
    ["@tag.attribute"] = "Identifier",
    ["@tag.delimiter"] = "Delimiter",
    ["@markup.strong"] = "markdownBold",
    ["@markup.italic"] = "markdownItalic",
    ["@markup.strikethrough"] = "DiagnosticDeprecated",
    ["@markup.link"] = "Underlined",
    ["@markup.link.label"] = "Identifier",
    ["@markup.link.url"] = "markdownUrl",
    ["@markup.raw"] = "markdownCode",
    ["@markup.raw.block"] = "markdownCodeBlock",
    ["@markup.quote"] = "markdownBlockquote",
    ["@markup.list"] = "markdownListMarker",
    ["@markup.math"] = "Special",
    ["@diff.plus"] = "Added",
    ["@diff.minus"] = "Removed",
    ["@diff.delta"] = "Changed",
  }

  for from, to in pairs(treesitter_links) do
    vim.api.nvim_set_hl(0, from, { link = to })
  end

  vim.api.nvim_set_hl(0, "Delimiter", { fg = separator_fg })
  vim.api.nvim_set_hl(0, "Operator", { fg = special_separator_fg })
  vim.api.nvim_set_hl(0, "@punctuation", { fg = separator_fg })
  vim.api.nvim_set_hl(0, "@punctuation.delimiter", { fg = separator_fg })
  vim.api.nvim_set_hl(0, "@punctuation.bracket", { fg = separator_fg })
  vim.api.nvim_set_hl(0, "@punctuation.special", { fg = special_separator_fg })
  vim.api.nvim_set_hl(0, "@operator", { fg = special_separator_fg })

  vim.api.nvim_set_hl(0, "@module", { fg = resolve_color("cyan", palette) })
  vim.api.nvim_set_hl(0, "@module.builtin", { fg = resolve_color("blue", palette) })
  vim.api.nvim_set_hl(0, "@variable", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@variable.builtin", { fg = resolve_color("red", palette) })
  vim.api.nvim_set_hl(0, "@variable.parameter", { fg = resolve_color("orange", palette), italic = true })
  vim.api.nvim_set_hl(0, "@variable.member", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@property", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@markup.heading", { fg = resolve_color("red", palette), bold = true })
  vim.api.nvim_set_hl(0, "@markup.heading.1", { fg = resolve_color("red", palette), bold = true })
  vim.api.nvim_set_hl(0, "@markup.heading.2", { fg = resolve_color("orange", palette), bold = true })
  vim.api.nvim_set_hl(0, "@markup.heading.3", { fg = resolve_color("syntax_fg", palette), bold = true })
  vim.api.nvim_set_hl(0, "@markup.heading.4", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@markup.heading.5", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@markup.heading.6", { fg = resolve_color("syntax_fg_dim", palette) })
  vim.api.nvim_set_hl(0, "@markup.raw.delimiter", { fg = resolve_color("modal_bg", palette) })
end

local function apply_semantic_highlights(palette)
  local semantic_links = {
    ["@lsp.type.class"] = "Type",
    ["@lsp.type.comment"] = "Comment",
    ["@lsp.type.decorator"] = "Macro",
    ["@lsp.type.enum"] = "Type",
    ["@lsp.type.enumMember"] = "Constant",
    ["@lsp.type.event"] = "Special",
    ["@lsp.type.function"] = "Function",
    ["@lsp.type.interface"] = "Type",
    ["@lsp.type.keyword"] = "Keyword",
    ["@lsp.type.macro"] = "Macro",
    ["@lsp.type.method"] = "Function",
    ["@lsp.type.modifier"] = "Keyword",
    ["@lsp.type.number"] = "Number",
    ["@lsp.type.operator"] = "Operator",
    ["@lsp.type.regexp"] = "String",
    ["@lsp.type.string"] = "String",
    ["@lsp.type.struct"] = "Type",
    ["@lsp.type.type"] = "Type",
    ["@lsp.type.typeParameter"] = "Type",
    ["@lsp.mod.deprecated"] = "DiagnosticDeprecated",
    ["@lsp.typemod.class.defaultLibrary"] = "Special",
    ["@lsp.typemod.enum.defaultLibrary"] = "Special",
    ["@lsp.typemod.enumMember.defaultLibrary"] = "Constant",
    ["@lsp.typemod.function.defaultLibrary"] = "Special",
    ["@lsp.typemod.method.defaultLibrary"] = "Special",
    ["@lsp.typemod.variable.defaultLibrary"] = "Special",
    ["@lsp.typemod.variable.global"] = "Constant",
    ["@lsp.typemod.variable.readonly"] = "Constant",
    ["@lsp.typemod.property.defaultLibrary"] = "Special",
    ["@lsp.typemod.property.readonly"] = "Constant",
  }

  for from, to in pairs(semantic_links) do
    vim.api.nvim_set_hl(0, from, { link = to })
  end

  vim.api.nvim_set_hl(0, "@lsp.type.namespace", { fg = resolve_color("cyan", palette) })
  vim.api.nvim_set_hl(0, "@lsp.type.parameter", { fg = resolve_color("orange", palette), italic = true })
  vim.api.nvim_set_hl(0, "@lsp.type.property", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@lsp.type.variable", { fg = resolve_color("syntax_fg", palette) })
  vim.api.nvim_set_hl(0, "@lsp.typemod.property.readonly", { fg = resolve_color("orange", palette) })
  vim.api.nvim_set_hl(0, "@lsp.typemod.variable.injected", { fg = resolve_color("purple", palette), italic = true })
  vim.api.nvim_set_hl(0, "@lsp.typemod.parameter.readonly", { fg = resolve_color("orange", palette), italic = true })
end

function M.apply()
  local palette, terminal_ansi_colors = derived_colors(vim.o.background == "light" and "light" or "dark")

  vim.cmd("highlight clear")

  if vim.fn.exists("syntax_on") == 1 then
    vim.cmd("syntax reset")
  end

  vim.g.colors_name = "taste"
  vim.g.terminal_ansi_colors = terminal_ansi_colors

  for _, item in ipairs(definitions.highlights) do
    local hl = {}

    hl.fg = resolve_color(item.fg, palette)
    hl.bg = resolve_color(item.bg, palette)
    hl.sp = resolve_color(item.sp, palette)
    apply_attributes(hl, item.attr)

    vim.api.nvim_set_hl(0, item.group, hl)
  end

  for _, link in ipairs(definitions.links) do
    vim.api.nvim_set_hl(0, link.from, { link = link.to })
  end

  apply_extra_highlights(palette)
  apply_treesitter_highlights(palette)
  apply_semantic_highlights(palette)
end

function M.lualine_theme()
  local palette = select(1, derived_colors(vim.o.background == "light" and "light" or "dark"))

  local function section(fg, bg, gui)
    local hl = {
      fg = resolve_color(fg, palette),
      bg = resolve_color(bg, palette),
    }

    if gui then hl.gui = gui end

    return hl
  end

  local function mode_section(bg)
    return {
      fg = to_hex(blend(palette.syntax_bg, palette[bg], 10)),
      bg = resolve_color(bg, palette),
      gui = "bold",
    }
  end

  local modal = section("syntax_fg", "modal_bg")
  local chrome = section("syntax_fg", "chrome_bg")
  local inactive = section("chrome_fg_dim", "chrome_bg")

  return {
    normal = {
      a = mode_section("green"),
      b = modal,
      c = chrome,
      x = chrome,
      y = modal,
      z = mode_section("green"),
    },
    insert = {
      a = mode_section("blue"),
      b = modal,
      c = chrome,
      x = chrome,
      y = modal,
      z = mode_section("blue"),
    },
    visual = {
      a = mode_section("orange"),
      b = modal,
      c = chrome,
      x = chrome,
      y = modal,
      z = mode_section("orange"),
    },
    replace = {
      a = mode_section("red"),
      b = modal,
      c = chrome,
      x = chrome,
      y = modal,
      z = mode_section("red"),
    },
    command = {
      a = mode_section("purple"),
      b = modal,
      c = chrome,
      x = chrome,
      y = modal,
      z = mode_section("purple"),
    },
    terminal = {
      a = mode_section("cyan"),
      b = modal,
      c = chrome,
      x = chrome,
      y = modal,
      z = mode_section("cyan"),
    },
    inactive = {
      a = inactive,
      b = inactive,
      c = inactive,
      x = inactive,
      y = inactive,
      z = inactive,
    },
  }
end

return M
