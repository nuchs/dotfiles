local function trouble_toggle()
  require("trouble").toggle()
end

local function trouble_workspace()
  require("trouble").open("workspace_diagnostics")
end
local function trouble_document()
  require("trouble").open("document_diagnostics")
end
local function trouble_quickfix()
  require("trouble").open("quickfix")
end
local function trouble_loclist()
  require("trouble").open("loclist")
end
local function trouble_references()
  require("trouble").open("lsp_references")
end

return {
  "folke/trouble.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  keys = {
    { "<Leader>xx", trouble_toggle, "Toggle trouble"},
    { "<Leader>xw", trouble_workspace, "Trouble in the workspace"},
    { "<Leader>xd", trouble_document, "Trouble in the document"},
    { "<Leader>xq", trouble_quickfix, "Trouble quicjfix list"},
    { "<Leader>xl", trouble_loclist, "Trouble location list"},
    { "gR", trouble_references, "Trouble references"},
  },
  opts = {},
}
