return {
  'mason-org/mason-lspconfig.nvim',
  dependencies = {
    'mason-org/mason.nvim',
    'neovim/nvim-lspconfig',
  },
  opts = {
    ensure_installed = {
      'bashls',
      'cssls',
      'docker-language-server',
      'emmet_ls',
      'gopls',
      'html',
      'jsonls',
      'lua_ls',
      'marksman',
      'yamlls',
    },
  },
}
