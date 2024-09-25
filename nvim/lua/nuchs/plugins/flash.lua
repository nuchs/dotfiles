return {
  'folke/flash.nvim',
  event = { 'BufRead', 'BufNewFile' },
  lazy = true,
  opts = {},
  keys = {
    {
      'r',
      mode = { 'o', 'x' },
      function()
        require('flash').jump()
      end,
      desc = 'Flash',
    },
    {
      's',
      mode = { 'n' },
      function()
        require('flash').jump()
      end,
      desc = 'Flash',
    },
  },
}
