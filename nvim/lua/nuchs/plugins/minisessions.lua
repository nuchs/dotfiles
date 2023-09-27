return {
  'echasnovski/mini.sessions',
  dependencies = {
    'stevearc/dressing.nvim',
  },
  version = false ,
  config = function()
    local mini = require('mini.sessions')
    mini.setup()

    local newSession = function()
      vim.ui.input(
        { prompt = 'Enter new session name: '},
        function(input)
          if(input ~= nil) then
            mini.write(input)
          end
        end
        )
    end

    local deleteSession = function()
      local sessions = {}
      for _, sess in pairs(mini.detected) do
        table.insert(sessions, sess.name)
      end

      vim.ui.select(
        sessions,
        {
          prompt = "Delete Session:"
        },
        function(choice)
          if(choice ~= nil) then
            mini.delete(choice)
          end
        end
      )
    end

    local keymap = vim.keymap
    keymap.set("n", "<Leader>ss", mini.select, { desc = "Choose session"})
    keymap.set("n", "<Leader>sc", newSession, { desc = "Create new session"})
    keymap.set("n", "<Leader>sd", deleteSession, { desc = "Delete session"})
  end
}
