return {
  'williamboman/mason.nvim',
  event = { 'BufReadPre', 'BufNewFile' },
  keys = {
    { '<Leader>m', '<Cmd>Mason<CR>', desc = 'Open Mason' },
  },
  lazy = true,
  opts = {
    ui = {
      icons = {
        package_installed = "✓",
        package_pending = "➜",
        package_uninstalled = "✗",
      }
    }
  },
}
