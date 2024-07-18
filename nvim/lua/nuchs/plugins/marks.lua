return {
  'chentoast/marks.nvim',
  event = { 'BufRead', 'BufNewFile' },
  config = function()
    require('marks').setup({
      refresh_interval = 250,
      force_write_shada = true,
    })
  end,
}
