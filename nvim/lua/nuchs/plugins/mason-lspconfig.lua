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
      'docker_compose_language_service',
      'dockerls',
      'emmet_ls',
      'gopls',
      'html',
      'htmx',
      'jsonls',
      'lua_ls',
      'marksman',
      'protols',
      'rust_analyzer',
      'taplo',
      'yamlls',
    },
  },
}
