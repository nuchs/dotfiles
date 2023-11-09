return {
  "echasnovski/mini.files",
  version = false,
  keys = {
    { "<Leader>e", "<Cmd>lua MiniFiles.open()<CR>", desc = "Open file explorer" },
  },
  config = function()
    local mini = require("mini.files")
    mini.setup({
      options = {
        use_as_default_explorer = true,
      },
    })
  end,
}
