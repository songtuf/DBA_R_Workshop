-- small-output.lua
-- Makes R output (results, warnings, messages) smaller and gray in Beamer
-- slides, so it reads as example output and the code stays the focus.
--
-- Use it by adding this to a slide file's header:
--   filters:
--     - small-output.lua
--
-- To adjust, change these two settings:
--   size:  \tiny, \scriptsize, \footnotesize, \small (code chunks use \scriptsize)
--   color: any xcolor name, e.g. darkgray, gray, black
local size  = "\\tiny"
local color = "darkgray"

function Div(el)
  if not FORMAT:match("beamer") then
    return nil
  end
  for _, class in ipairs(el.classes) do
    if class:match("^cell%-output") then
      table.insert(el.content, 1,
        pandoc.RawBlock("latex", "\\begingroup" .. size .. "\\color{" .. color .. "}"))
      table.insert(el.content, pandoc.RawBlock("latex", "\\endgroup"))
      return el
    end
  end
end
