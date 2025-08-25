return {
  'echasnovski/mini.files',
  version = false,
  lazy = true,
  keys = {
    { '<Leader>e', '<Cmd>lua MiniFiles.open()<CR>', desc = 'Open file explorer' },
  },
  config = function()
    local mf = require('mini.files')
    mf.setup({
      windows = {
        preview = true,
        width_preview = 80,
      },
    })

    local map_split = function(buf_id, lhs, direction)
      local rhs = function()
        local cur_target = mf.get_explorer_state().target_window
        local new_target = vim.api.nvim_win_call(cur_target, function()
          vim.cmd(direction .. ' split')
          return vim.api.nvim_get_current_win()
        end)

        mf.set_target_window(new_target)
        mf.go_in({ close_on_file = true })
      end

      local desc = 'Split ' .. direction
      vim.keymap.set('n', lhs, rhs, { buffer = buf_id, desc = desc })
    end

    vim.api.nvim_create_autocmd('User', {
      pattern = 'MiniFilesBufferCreate',
      callback = function(args)
        local buf_id = args.data.buf_id
        map_split(buf_id, '<C-s>', 'belowright horizontal')
        map_split(buf_id, '<C-v>', 'belowright vertical')
      end,
    })

    vim.cmd('highlight MiniFilesTitleFocused guifg=#FFD700')
  end,
}
