-- Preserve semantic HTML Divs while using compact LaTeX blocks for PDF.
local function has_class(el, class_name)
  for _, class in ipairs(el.classes or {}) do
    if class == class_name then return true end
  end
  return false
end

local function child_div(el, class_name)
  for _, block in ipairs(el.content or {}) do
    if block.t == "Div" then
      if has_class(block, class_name) then return block end
      local nested = child_div(block, class_name)
      if #nested.content > 0 then return nested end
    end
  end
  return pandoc.Div({})
end

local function blocks_to_latex(blocks)
  local text = pandoc.write(pandoc.Pandoc(blocks or {}), "latex")
  return text:gsub("^%s+", ""):gsub("%s+$", "")
end

function Div(el)
  if not FORMAT:match("latex") then return nil end
  if has_class(el, "cv-header") then
    local name = blocks_to_latex(child_div(el, "cv-name").content)
    local details = blocks_to_latex(child_div(el, "cv-details").content)
    return pandoc.RawBlock("latex", string.format("\\cvheader{%s}{%s}{imgs/profile.jpg}", name, details))
  end
  if has_class(el, "cv-entry") then
    local date = blocks_to_latex(child_div(el, "cv-date").content)
    local body = blocks_to_latex(child_div(el, "cv-body").content)
    return pandoc.RawBlock("latex", string.format("\\cvitemblock{%s}{%s}", date, body))
  end
  return nil
end
