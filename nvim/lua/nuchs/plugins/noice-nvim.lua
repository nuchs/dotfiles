return {
  'folke/noice.nvim',
  event = 'VeryLazy',
  enabled = true,
  dependencies = {
    'MunifTanjim/nui.nvim',
    'rcarriga/nvim-notify',
    'nvim-treesitter/nvim-treesitter',
    'hrsh7th/nvim-cmp',
  },
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
}
