local ls = require("luasnip")
local t = ls.text_node
local i = ls.insert_node

return {
  ls.snippet(
    { trig = "tr", dscr = "Test runner" },
    {
      t("t.Run(\""),
      i(1),
      t({ "\", func(t * testing.T) {", "" }),
      i(0),
      t({ "", "})" })
    }
  ),
}
