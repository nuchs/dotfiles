return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.3',
  lazy = true,
  keys = {
    { '<Leader>t',  '<Cmd>Telescope<CR>',                           desc = 'Open Telescope' },
    { '<Leader>fb', '<Cmd>Telescope buffers<CR>',                   desc = 'Select buffer' },
    { '<Leader>ff', '<Cmd>Telescope fd<CR>',                        desc = 'Find files in cwd' },
    { '<Leader>fr', '<Cmd>Telescope oldfiles<CR>',                  desc = 'Mru files' },
    { '<Leader>//', '<Cmd>Telescope live_grep<CR>',                 desc = 'Search in cwd' },
    { '<Leader>/',  '<Cmd>Telescope current_buffer_fuzzy_find<CR>', desc = 'Search current buffer' },
    { '<Leader>d',  '<Cmd>Telescope diagnostics<CR>',               desc = 'diagnostics' },
    { '<Leader>h',  '<Cmd>Telescope help_tags<CR>',                 desc = 'Search help' },
    { '<Leader>k',  '<Cmd>Telescope keymaps<CR>',                   desc = 'List keymaps' },
    { '<Leader>p',  '<Cmd>Telescope luasnip<CR>',                   desc = 'Search snippets' },
  },
  cmd = { 'Telescope' },
  dependencies = {
    'nvim-lua/plenary.nvim',
    'benfowler/telescope-luasnip.nvim',
    { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  },
  config = function()
    local telescope = require('telescope')
    telescope.setup({
      pickers = {
        buffers = {
          mappings = {
            n = {
              ['<C-d>'] = require('telescope.actions').delete_buffer,
            },
            i = {
              ['<C-d>'] = require('telescope.actions').delete_buffer,
            },
          },
        },
      },
    })
    telescope.load_extension('luasnip')
    telescope.load_extension('fzf')
  end,
}
