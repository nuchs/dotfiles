return {
  "echasnovski/mini.sessions",
  dependencies = {
    "stevearc/dressing.nvim",
  },
  version = false,
  config = function()
    local mini = require("mini.sessions")

    mini.setup()

    local newSession = function()
      vim.ui.input({ prompt = "Enter new session name: " }, function(input)
        if input ~= nil then
          mini.write(input)
        end
      end)
    end

    local deleteSession = function(prompt_bufnr)
      local picker = require("telescope.actions.state").get_current_picker(prompt_bufnr)
      picker:delete_selection(function(selection)
        mini.delete(selection.value)
      end)
    end

    local closeSession = function()
      local starter = require("mini.starter")
      mini.write(nil, { force = true })
      vim.v.this_session = ''
      vim.cmd "only"
      vim.cmd("%bwipeout")
      starter.open()
    end

    require("dressing").setup({
      select = {
        telescope = {
          layout_config = {
            width = 80,
            height = 20,
          },
        },
        get_config = function(opts)
          if opts.prompt == "Select session to read" then
            return {
              telescope = {
                attach_mappings = function(_, map)
                  map("i", "<C-d>", deleteSession)
                  return true
                end,
              },
            }
          end
        end,
      },
    })

    local keymap = vim.keymap
    keymap.set("n", "<Leader>ss", mini.select, { desc = "Manage sessions" })
    keymap.set("n", "<Leader>sc", newSession, { desc = "Create new session" })
    keymap.set("n", "<Leader>sq", closeSession, { desc = "Close the current session" })
  end,
}
