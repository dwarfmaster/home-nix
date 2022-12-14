
function get_asciidoc_link()
  -- Finding if there is an url under the cursor
  local line = vim.api.nvim_get_current_line()
  local _, c = unpack(vim.api.nvim_win_get_cursor(0))
  local pattern = "(%a+):(%S*)%[([^%]]+)%]"
  local first, last, kind, uri, args
  repeat
    first, last, kind, uri, args = string.find(line, pattern)
    if first == nil then return nil end
  until(first <= c and c <= last)

  -- TODO consider splitting args on ','

  -- Found match on cursor
  -- TODO actually doing something with it
  print(string.format("Found link of kind %s", kind))
end
