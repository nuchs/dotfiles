-- Utility function to safely load module {{{1
local init = {}
function init.load(module)
  local ok, err = pcall(require, module)

  if not ok then
    print("Failed to load " .. module)
    print(err)
    return
  end
end

-- My Lua configuration {{{1
init.load("user.options")
init.load("user.keymaps")
init.load("user.autocommands")
init.load("user.plugins")


-- TODO {{{1
-- configure completion 
-- diagnostics (trouble nvim?)
-- debug
-- project/session management
-- Configure mason
--  * linters
--  * debuggers
--  * language servers (including emmet)
-- snippets - get it working
-- testing
-- Configure status line
-- Configure noice
-- treesitter plugins
-- transparency
