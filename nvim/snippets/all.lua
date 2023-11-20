local ls = require("luasnip")
local t = ls.text_node
local i = ls.insert_node

return {
  ls.snippet(
    { trig = "hi", dscr = "Standard greeting" },
    { t("Hello, World!"), i(0) }
  ),
}
