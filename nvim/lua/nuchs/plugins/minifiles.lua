return {
  'echasnovski/mini.files',
  version = false,
  lazy = true,
  keys = {
    { '<Leader>e', '<Cmd>lua MiniFiles.open()<CR>', desc = 'Open file explorer' },
  },
  config = function()
    require('mini.files').setup({
      windows = {
        preview = true,
        width_preview = 80,
      },
    })

    vim.cmd('highlight MiniFilesTitleFocused guifg=#FFD700')
  end,
}
