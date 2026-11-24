local M = {}
local terminal = "com.superlogical.terminal"
local tail_bytes = 8192

function M.normalize_id(value)
  local id = value:match("^%s*(.-)%s*$"):lower()
  local a, b, c, d, e = id:match("^(%x+)%-(%x+)%-(%x+)%-(%x+)%-(%x+)$")
  if a and #a == 8 and #b == 4 and #c == 4 and #d == 4 and #e == 12 then
    return id
  end
end

-- Only inspect the live footer, never UUIDs mentioned in the conversation.
-- This matches the Context and session-id items in our Codex status_line.
function M.footer_id(text)
  local lines = {}
  for line in text:gmatch("[^\r\n]+") do
    if line:find("%S") then
      lines[#lines + 1] = line
    end
  end
  for index = #lines, math.max(1, #lines - 2), -1 do
    local line = lines[index]
    if line:match("^  %S") and line:find(" · Context ", 1, true) then
      local id = line:match(" · ([%x%-]+)%s*$")
      if id and M.normalize_id(id) then return id:lower(), false end
      local prefix = line:match(" · ([%x%-]+)…%s*$")
      if prefix and #prefix >= 8 and #prefix < 36 then
        return prefix:lower(), true
      end
    end
  end
end

local function block_call(pane, method, args)
  return rex.call(terminal .. "." .. method, {
    session_id = pane.session_id,
    block_id = pane.block_id,
    args = args or {},
  })
end

local function read_id(pane)
  local process = block_call(pane, "process")
  local foreground = process and process.foreground
  if not foreground or foreground.name ~= "codex" then return end
  pane.cwd = foreground.cwd

  -- format reports the snapshot's size, so large scrollbacks need only one
  -- small read at the end. Do not search or transfer the whole conversation.
  local result = block_call(pane, "format", {
    format = "text", trim = true, unwrap = true, max_bytes = 1,
  })
  if not result then return end
  local text = result.content
  if result.id then
    if not result.total then return end
    local tail = block_call(pane, "blob_read", {
      id = result.id,
      offset = math.max(0, result.total - tail_bytes),
      max_bytes = tail_bytes * 8,
    })
    if not tail or not tail.eof then return end
    text = tail.data
  end
  return M.footer_id(text or "")
end

local function unique_local_id(prefix)
  -- Codex shortens the last status-line item in narrow panes. Resolve that
  -- prefix only when its local thread index contains exactly one matching ID.
  -- prefix comes from footer_id's hex/hyphen pattern, never raw user input.
  local home = os.getenv("CODEX_HOME") or (os.getenv("HOME") .. "/.codex")
  local result = rex.exec({
    "sqlite3", "-readonly", home .. "/state_5.sqlite",
    "SELECT id FROM threads WHERE id LIKE '" .. prefix .. "%' LIMIT 2;",
  }, { timeout = 2 })
  if not result or not result.ok then return end
  local ids = {}
  for line in result.stdout:gmatch("[^\r\n]+") do
    local id = M.normalize_id(line)
    if not id then return end
    ids[#ids + 1] = id
  end
  if #ids == 1 then return ids[1] end
end

local function matches(pane, id)
  local visible, shortened = read_id(pane)
  if not visible then return false end
  if not shortened then return visible == id end
  return id:sub(1, #visible) == visible and unique_local_id(visible) == id
end

-- Keep existing app subscriptions intact. CLI callers may need a temporary
-- attachment; use rex.call because config actions lack attach/detach helpers.
local function open_session(args)
  local view = rex.call("session.view", args)
  if view then return view, false end
  local joined, err = rex.call("session.attach", args)
  return joined and joined.view, joined ~= nil, err
end

local function release_session(args, attached)
  if attached then rex.call("session.detach", args) end
end

function M.find(id)
  id = M.normalize_id(id)
  if not id then error("Enter a full Codex session UUID.", 0) end
  local sessions, err = rex.call("session.list", {})
  if not sessions then error(err, 0) end
  local found = {}
  for _, session in ipairs(sessions.sessions or {}) do
    local session_args = { session_id = session.session_id }
    local view, attached, view_err = open_session(session_args)
    if not view then error(view_err, 0) end
    local blocks, blocks_err = rex.call("session.list_blocks", session_args)
    if not blocks then
      release_session(session_args, attached)
      error(blocks_err, 0)
    end
    local tabs = {}
    for index, window in ipairs(view.windows or {}) do
      tabs[window.window_id] = index
    end
    for _, block in ipairs(blocks.blocks or {}) do
      if block.creator_name == terminal and block.placement == "placed" then
        local pane = {
          session_id = session.session_id,
          session_label = session.label,
          block_id = block.block_id,
          window_id = block.window_id,
          tab_index = tabs[block.window_id],
          label = block.label,
        }
        if matches(pane, id) then found[#found + 1] = pane end
      end
    end
    release_session(session_args, attached)
  end
  return found
end

function M.setup()
  rex.action {
    name = "find_codex_session",
    title = "Find Codex Session…",
    description = "Paste a Codex session UUID to focus its open pane.",
    keywords = "codex session uuid find jump pane",
    args = { id = "string?" },
    run = function(_, args)
      local input = args.id or rex.ui.input {
        title = "Find Codex Session",
        placeholder = "Paste a session UUID",
        confirm = "Go to pane",
      }
      if input == nil then return end
      local id = M.normalize_id(input)
      if not id then error("Enter a full Codex session UUID.", 0) end
      local panes = M.find(id)
      if #panes == 0 then
        error("No open Codex pane found for " .. id ..
          ". Make sure its session ID is visible in the status line; widen the pane if needed.", 0)
      end

      local pane = panes[1]
      if #panes > 1 then
        local items = {}
        for index, candidate in ipairs(panes) do
          items[index] = {
            id = tostring(index),
            title = candidate.session_label .. " · Tab " .. candidate.tab_index,
            subtitle = candidate.cwd or candidate.label,
            icon = "terminal",
          }
        end
        local selected = rex.ui.pick { title = "Choose Codex Pane", items = items }
        if selected == nil then return end
        pane = panes[tonumber(selected)]
      end

      -- The session may have changed while the picker was open.
      local view, attached, err = open_session { session_id = pane.session_id }
      if not view then error(err, 0) end
      if not matches(pane, id) then
        release_session({ session_id = pane.session_id }, attached)
        error("That pane is no longer showing this Codex session. Try again.", 0)
      end
      local focused, err = rex.session.focus_block {
        session_id = pane.session_id, block_id = pane.block_id,
      }
      release_session({ session_id = pane.session_id }, attached)
      if not focused then error(err, 0) end
      rex.client.queue("session.select", {
        session_id = pane.session_id, window_id = pane.window_id,
      })
      return { id = id, session_id = pane.session_id, block_id = pane.block_id }
    end,
  }
end

return M
