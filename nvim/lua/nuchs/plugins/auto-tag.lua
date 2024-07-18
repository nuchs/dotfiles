return {
  'windwp/nvim-ts-autotag',
  ft = {
    'htm',
    'html',
    'js',
    'jsx',
    'md',
    'mjs',
    'ts',
    'tsx',
    'xml',
  },
  config = function()
    require('nvim-ts-autotag').setup({})
  end,
}
