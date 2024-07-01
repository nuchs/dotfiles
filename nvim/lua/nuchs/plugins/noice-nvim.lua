return {
  'folke/noice.nvim',
  event = 'VeryLazy',
  opts = {
    presets = {
      command_palette = true,
    },
    routes = {
      {
        view = 'notify',
        filter = {
          event = 'msg_showmode',
          find = 'record',
        },
      },
      {
        filter = {
          event = 'msg_show',
          kind = '',
          find = 'written',
        },
        opts = { skip = true },
      },
    },
    lsp = {
      progress = { enabled = true },
    },
  },
  dependencies = {
    'MunifTanjim/nui.nvim',
    'rcarriga/nvim-notify',
    'nvim-treesitter/nvim-treesitter',
  },
}
