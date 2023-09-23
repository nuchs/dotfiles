return {
  'nvim-telescope/telescope.nvim', 
  tag = '0.1.3',
  dependencies = { 
    'nvim-lua/plenary.nvim' ,
    'jvgrootveld/telescope-zoxide',
    'benfowler/telescope-luasnip.nvim',
    'nvim-telescope/telescope-ui-select.nvim',
  },
  config = function ()
    local telescope = require('telescope')
    telescope.setup()
    telescope.load_extension('zoxide')
    telescope.load_extension('luasnip')
    telescope.load_extension('ui-select')
  end,
}
