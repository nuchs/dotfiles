return {
  'folke/flash.nvim',
  event = { 'BufRead', 'BufNewFile' },
  lazy = true,
  opts = {},
  keys = {
    {
      's',
      mode = { 'n', 'o', 'x' },
      function()
        require('flash').jump()
      end,
      desc = 'Flash',
    },
    {
      'C-s',
      mode = 'o',
      function()
        require('flash').jump()
      end,
      desc = 'Flash',
    },
    {
      'C-R',
      mode = 'o',
      function()
        require('flash').remote()
      end,
      desc = 'Remote Flash',
    },
  },
}
