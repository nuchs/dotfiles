return {
  'echasnovski/mini.files',
  version = false,
  lazy = true,
  keys = {
    { '<Leader>e', '<Cmd>lua MiniFiles.open()<CR>', desc = 'Open file explorer' },
  },
  dependencies = {
    'nuchs/neovim-session-manager',
  },
  config = function()
    local mini = require('mini.files')
    mini.setup({
      options = {
        use_as_default_explorer = true,
      },
      windows = {
        preview = true,
        width_preview = 80,
      },
    })

    vim.cmd('highlight MiniFilesTitleFocused guifg=#FFD700')
  end,
}
