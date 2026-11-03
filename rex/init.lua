-- Match the active tmux configuration: Ctrl+A prefix and one-based tabs.
-- Reload with Rex's Reload Config action or `rex config reload`.
-- Rex has no native tmux copy mode, last-window/last-pane action, buffer
-- picker, or preset-layout cycle. Those keys are intentionally unassigned.

local prefix = "ctrl+a>"
local function bind(key, action, args)
  rex.bind(prefix .. key, action, args)
end

-- Press Ctrl+A twice to send Ctrl+A to the terminal.
bind("ctrl+a", "pane.send_key", { key = "ctrl+a" })

-- Match tmux: Ctrl+Enter sends Enter instead of an extended key sequence.
rex.bind("ctrl+enter", "pane.send_key", { key = "enter" })

-- Windows in tmux are tabs in Rex.
bind("c", "client.tab.new")
bind("n", "client.tab.next")
bind("p", "client.tab.previous")
bind(",", "client.tab.rename")
bind("shift+7", "client.tab.close") -- &
for index = 1, 9 do
  bind(tostring(index), "client.tab.goto", { index = index })
end
rex.bind("alt+n", "client.tab.next")
rex.bind("alt+p", "client.tab.previous")

-- Splits: custom | and -, plus tmux's default % and ".
-- Rex spells shifted symbols using the unshifted key on a US keyboard.
bind("shift+\\", "pane.split", { direction = "right" }) -- |
bind("-", "pane.split", { direction = "down" })
bind("shift+5", "pane.split", { direction = "right" }) -- %
bind("shift+'", "pane.split", { direction = "down" }) -- "
bind("z", "pane.zoom")
bind("x", "pane.close")
bind("o", "pane.focus_next")
bind("shift+1", "pane.move_to_new_tab") -- !
bind("shift+e", "pane.balance")

-- Preserve Vim's own Alt+H/J/K/L split navigation when Vim is focused.
local terminal = "com.superlogical.terminal"
local vim_names = {
  vi = true, vim = true, nvim = true, lvim = true, view = true,
  nview = true, gvim = true, gview = true, vimdiff = true,
  nvimdiff = true, gvimdiff = true, viewdiff = true,
}

local function vim_focused()
  local process = rex.block.call(terminal, "process", {})
  if not process or not process.foreground then
    return nil
  end
  local foreground = process.foreground
  for _, value in ipairs({ foreground.name or "", foreground.argv0 or "",
      foreground.path or "" }) do
    local name = value:match("([^/]+)$") or value
    if vim_names[name:lower()] then
      return true
    end
  end
  return false
end

rex.action {
  name = "tmux_navigate",
  title = "Navigate Pane (Vim Aware)",
  args = { direction = "string", key = "string" },
  repeats = true,
  run = function(ctx, args)
    -- If process detection fails, preserve the terminal's key handling.
    if vim_focused() ~= false then
      rex.client.queue("pane.send_key", { key = args.key })
    else
      rex.client.queue("pane.focus", { direction = args.direction })
    end
  end,
}

for _, entry in ipairs({
  { "h", "left" }, { "j", "down" },
  { "k", "up" }, { "l", "right" },
}) do
  local key, direction = entry[1], entry[2]
  bind(direction, "pane.focus", { direction = direction })
  rex.bind("alt+" .. key, "tmux_navigate", {
    direction = direction, key = "alt+" .. key,
  })

  -- Rex resizes by percentage, whereas tmux resizes by terminal cells.
  bind("shift+" .. key, "pane.resize", { direction = direction, amount = 5 })
  bind("alt+" .. key, "pane.resize", { direction = direction, amount = 1 })
  bind("alt+" .. direction, "pane.resize", { direction = direction, amount = 5 })
  bind("ctrl+" .. direction, "pane.resize", { direction = direction, amount = 1 })
end

-- Ctrl+L clears terminal history at the shell and remains native in Vim.
rex.action {
  name = "tmux_clear",
  title = "Clear Terminal (Vim Aware)",
  run = function(ctx, args)
    if vim_focused() == false then
      local result, err = rex.block.call(terminal, "clear", {})
      if err ~= nil then error(err, 0) end
    end
    rex.client.queue("pane.send_key", { key = "ctrl+l" })
  end,
}
rex.bind("ctrl+l", "tmux_clear")

-- Sessions and configuration.
bind("s", "session.switch")
bind("ctrl+j", "session.switch")
bind("shift+9", "session.previous") -- (
bind("shift+0", "session.next") -- )
bind("shift+4", "session.rename") -- $
bind("shift+;", "client.palette.toggle") -- :
bind("r", "client.config.reload")
