local function EditSnippets()
  require('luasnip.loaders').edit_snippet_files()
end

return {
  'L3MON4D3/LuaSnip',
  version = '2.*',
  build = 'make install_jsregexp',
  lazy = true,
  event = 'InsertEnter',
  dependencies = { 'rafamadriz/friendly-snippets' },
  keys = {
    { '<Leader>P', EditSnippets, desc = 'Edit snippets' },
  },
  config = function()
    require('luasnip.loaders.from_vscode').lazy_load()
    require('luasnip.loaders.from_lua').lazy_load({ paths = '~/.config/nvim/snippets/' })
  end,
}
