local M = {}

local function buffer_items()
  local cwd = vim.uv.cwd() or vim.fn.getcwd()
  local current = vim.api.nvim_get_current_buf()
  local items = {}

  for _, info in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
    local name = info.name ~= '' and info.name or string.format('[No Name] #%d', info.bufnr)
    local basename = info.name ~= '' and vim.fn.fnamemodify(info.name, ':t') or '[No Name]'
    local directory = info.name ~= '' and vim.fn.fnamemodify(info.name, ':h') or ''
    local relative_path = info.name ~= '' and vim.fn.fnamemodify(info.name, ':.') or basename
    local extension = info.name ~= '' and vim.fn.fnamemodify(info.name, ':e') or ''

    if info.name ~= '' and vim.startswith(info.name, cwd .. '/') then
      relative_path = info.name:sub(#cwd + 2)
      directory = vim.fn.fnamemodify(relative_path, ':h')
      if directory == '.' then directory = '' end
    elseif directory == '.' then
      directory = ''
    end

    items[#items + 1] = {
      bufnr = info.bufnr,
      path = info.name ~= '' and info.name or string.format('buffer://%d', info.bufnr),
      preview_key = tostring(info.bufnr),
      preview_title = relative_path,
      relative_path = relative_path,
      name = basename,
      extension = extension,
      directory = directory,
      current = info.bufnr == current,
      visible = #info.windows > 0,
      modified = info.changed == 1,
      lastused = info.lastused or 0,
      cursor_line = info.lnum or 1,
    }
  end

  table.sort(items, function(a, b)
    if a.current ~= b.current then return a.current end
    if a.visible ~= b.visible then return a.visible end
    if a.modified ~= b.modified then return a.modified end
    if a.lastused ~= b.lastused then return a.lastused > b.lastused end
    return a.bufnr < b.bufnr
  end)

  return items
end

local function fuzzy_positions(haystack, needle)
  if needle == '' then return {}, 0 end

  haystack = haystack:lower()
  needle = needle:lower()

  local positions = {}
  local score = 0
  local search_from = 1

  for idx = 1, #needle do
    local char = needle:sub(idx, idx)
    local found = haystack:find(char, search_from, true)
    if not found then return nil end

    positions[#positions + 1] = found
    score = score + 1
    if found == search_from then score = score + 4 end
    if found == 1 or haystack:sub(found - 1, found - 1):match('[%s/_%-%.]') then score = score + 6 end
    if idx > 1 and positions[#positions - 1] == found - 1 then score = score + 8 end

    search_from = found + 1
  end

  return positions, score
end

local function search_buffers(opts)
  local query = opts.query or ''
  local page_size = opts.page_size or 100
  local page_index = opts.page_index or 0
  local all_items = buffer_items()
  local matches = {}

  for _, item in ipairs(all_items) do
    if query == '' then
      matches[#matches + 1] = item
    else
      local basename_positions, basename_score = fuzzy_positions(item.name, query)
      local path_positions, path_score = fuzzy_positions(item.relative_path, query)

      if basename_positions then
        item.match_positions = basename_positions
        item.score = basename_score + 50
        matches[#matches + 1] = item
      elseif path_positions then
        item.match_positions = nil
        item.score = path_score
        matches[#matches + 1] = item
      end
    end
  end

  table.sort(matches, function(a, b)
    local left = a.score or 0
    local right = b.score or 0
    if left ~= right then return left > right end
    if a.current ~= b.current then return a.current end
    if a.visible ~= b.visible then return a.visible end
    if a.modified ~= b.modified then return a.modified end
    if a.lastused ~= b.lastused then return a.lastused > b.lastused end
    return a.bufnr < b.bufnr
  end)

  local total_matched = #matches
  local start_idx = page_index * page_size + 1
  local end_idx = math.min(start_idx + page_size - 1, total_matched)
  local page_items = {}

  for idx = start_idx, end_idx do
    if matches[idx] then page_items[#page_items + 1] = matches[idx] end
  end

  local status_info
  if query == '' then
    status_info = tostring(#all_items)
  else
    status_info = string.format('%d/%d', total_matched, #all_items)
  end

  return {
    items = page_items,
    total_matched = total_matched,
    status_info = status_info,
  }
end

local renderer = {}

function renderer.render_line(item, ctx)
  local icons = require('fff.file_picker.icons')
  local icon, _ = icons.get_icon(item.name, item.extension, false)
  local lines = {}

  local suffix_parts = { string.format(' #%d', item.bufnr) }
  if item.modified then suffix_parts[#suffix_parts + 1] = ' [+]' end
  if item.visible then suffix_parts[#suffix_parts + 1] = ' [win]' end
  local suffix = table.concat(suffix_parts, '')

  local icon_width = icon and (vim.fn.strdisplaywidth(icon) + 1) or 0
  local available_width = math.max(ctx.max_path_width - icon_width - vim.fn.strdisplaywidth(suffix), 30)
  local filename, dir_path = ctx.format_file_display(item, available_width)

  local line = icon and string.format('%s %s %s%s', icon, filename, dir_path, suffix)
    or string.format('%s %s%s', filename, dir_path, suffix)
  local padding = math.max(0, ctx.win_width - vim.fn.strdisplaywidth(line) + 5)
  lines[#lines + 1] = line .. string.rep(' ', padding)

  return lines
end

function renderer.apply_highlights(item, ctx, item_idx, buf, ns_id, line_idx, _)
  local icons = require('fff.file_picker.icons')
  local is_cursor = ctx.cursor == item_idx
  local icon, icon_hl = icons.get_icon(item.name, item.extension, false)
  local icon_width = icon and vim.fn.strdisplaywidth(icon) or 0
  local suffix_parts = { string.format(' #%d', item.bufnr) }
  if item.modified then suffix_parts[#suffix_parts + 1] = ' [+]' end
  if item.visible then suffix_parts[#suffix_parts + 1] = ' [win]' end
  local suffix = table.concat(suffix_parts, '')
  local available_width = math.max(ctx.max_path_width - (icon and (icon_width + 1) or 0) - vim.fn.strdisplaywidth(suffix), 30)
  local filename, dir_path = ctx.format_file_display(item, available_width)

  if is_cursor then
    vim.api.nvim_buf_set_extmark(buf, ns_id, line_idx - 1, 0, {
      end_col = 0,
      end_row = line_idx,
      hl_group = ctx.config.hl.cursor,
      hl_eol = true,
      priority = 100,
    })
  end

  if icon and icon_hl then
    vim.api.nvim_buf_set_extmark(buf, ns_id, line_idx - 1, 0, {
      end_col = icon_width,
      hl_group = icon_hl,
    })
  end

  local dir_start = #filename + 1
  if icon then dir_start = dir_start + #icon + 1 end
  if #dir_path > 0 then
    vim.api.nvim_buf_set_extmark(buf, ns_id, line_idx - 1, dir_start, {
      end_col = dir_start + #dir_path,
      hl_group = ctx.config.hl.directory_path,
    })
  end

  if item.current then
    vim.api.nvim_buf_set_extmark(buf, ns_id, line_idx - 1, 0, {
      virt_text = { { ' (current)', is_cursor and ctx.config.hl.cursor or 'Comment' } },
      virt_text_pos = 'right_align',
    })
  end

  if ctx.query ~= '' and item.match_positions then
    local base_col = icon and (icon_width + 1) or 0
    for _, pos in ipairs(item.match_positions) do
      vim.api.nvim_buf_set_extmark(buf, ns_id, line_idx - 1, base_col + pos - 1, {
        end_col = base_col + pos,
        hl_group = ctx.config.hl.matched or 'IncSearch',
      })
    end
  end
end

local function preview_buffer(item, preview_buf, preview_win)
  local lines
  if vim.api.nvim_buf_is_valid(item.bufnr) then
    lines = vim.api.nvim_buf_get_lines(item.bufnr, 0, -1, false)
  else
    lines = { 'Buffer is no longer valid.' }
  end

  if #lines == 0 then lines = { '' } end

  vim.api.nvim_set_option_value('modifiable', true, { buf = preview_buf })
  vim.api.nvim_buf_set_lines(preview_buf, 0, -1, false, lines)
  vim.api.nvim_set_option_value('modifiable', false, { buf = preview_buf })

  if vim.api.nvim_buf_is_valid(item.bufnr) then
    vim.api.nvim_set_option_value('filetype', vim.bo[item.bufnr].filetype, { buf = preview_buf })
  end

  vim.api.nvim_set_option_value('number', true, { win = preview_win })
  vim.api.nvim_set_option_value('cursorline', true, { win = preview_win })
  vim.api.nvim_win_set_cursor(preview_win, { math.max(1, item.cursor_line), 0 })
end

local function select_buffer(item, action)
  if action == 'split' then
    vim.cmd('sbuffer ' .. item.bufnr)
  elseif action == 'vsplit' then
    vim.cmd('vert sbuffer ' .. item.bufnr)
  elseif action == 'tab' then
    vim.cmd('tab split | buffer ' .. item.bufnr)
  else
    vim.cmd('buffer ' .. item.bufnr)
  end
end

local function buffer_item_key(item) return string.format('buffer:%d', item.bufnr) end

local function buffers_to_quickfix(ctx)
  local items = (#ctx.selected_items > 0) and ctx.selected_items or ctx.items
  local qf_list = {}

  for _, item in ipairs(items) do
    if item and item.bufnr and vim.api.nvim_buf_is_valid(item.bufnr) then
      qf_list[#qf_list + 1] = {
        bufnr = item.bufnr,
        lnum = math.max(1, item.cursor_line or 1),
        col = 1,
        text = item.relative_path,
      }
    end
  end

  return qf_list
end

local source = {
  search = search_buffers,
  preview = preview_buffer,
  select = select_buffer,
  item_key = buffer_item_key,
  to_quickfix = buffers_to_quickfix,
}

function M.open()
  require('fff').pick({
    title = 'Buffers',
    source = source,
    renderer = renderer,
  })
end

return M
