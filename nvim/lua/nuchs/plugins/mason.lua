local function on_attach_standard(_, bufnr)
  local keymap = vim.keymap
  local opts = { noremap = true, silent = true, buffer = bufnr }

  opts.desc = 'Show LSP references'
  keymap.set('n', 'gr', '<cmd>Telescope lsp_references<CR>', opts)

  opts.desc = 'Go to declaration'
  keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)

  opts.desc = 'Show LSP definitions'
  keymap.set('n', 'gd', '<cmd>Telescope lsp_definitions<CR>', opts)

  opts.desc = 'Show LSP implementations'
  keymap.set('n', 'gi', '<cmd>Telescope lsp_implementations<CR>', opts)

  opts.desc = 'Show LSP type definitions'
  keymap.set('n', 'gt', '<cmd>Telescope lsp_type_definitions<CR>', opts)

  opts.desc = 'See available code actions'
  keymap.set({ 'n', 'v' }, '<C-.>', vim.lsp.buf.code_action, opts)

  opts.desc = 'Smart rename'
  keymap.set('n', '<F2>', vim.lsp.buf.rename, opts)

  opts.desc = 'Go to previous diagnostic'
  keymap.set('n', '[[', vim.diagnostic.goto_prev, opts)

  opts.desc = 'Go to next diagnostic'
  keymap.set('n', ']]', vim.diagnostic.goto_next, opts)

  opts.desc = 'Show documentation for what is under cursor'
  keymap.set('n', 'K', vim.lsp.buf.hover, opts)
end

return {
  'williamboman/mason.nvim',
  event = { 'BufReadPre', 'BufNewFile' },
  keys = {
    { '<Leader>m', '<Cmd>Mason<CR>', desc = 'Open Mason' },
  },
  lazy = true,
  dependencies = {
    'williamboman/mason-lspconfig.nvim',
    'hrsh7th/cmp-nvim-lsp',
    'neovim/nvim-lspconfig',
  },
  config = function()
    require('mason').setup({
      ui = {
        icons = {
          package_installed = '✓',
          package_pending = '➜',
          package_uninstalled = '✗',
        },
      },
    })

    local signs = { Error = ' ', Warn = ' ', Hint = '󰠠 ', Info = ' ' }
    for type, icon in pairs(signs) do
      local hl = 'DiagnosticSign' .. type
      vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = '' })
    end

    vim.diagnostic.setqflist()

    local lspconfig = require('lspconfig')
    local capabilities = require('cmp_nvim_lsp').default_capabilities()
    local mason_lspconfig = require('mason-lspconfig')

    mason_lspconfig.setup()
    mason_lspconfig.setup_handlers({
      -- Default handler to automatically setup a new language server
      function(server_name)
        lspconfig[server_name].setup({
          capabilities = capabilities,
          on_attach = on_attach_standard,
        })
      end,

      -- Config for specific language servers
      ['lua_ls'] = function()
        lspconfig.lua_ls.setup({
          capabilities = capabilities,
          on_attach = on_attach_standard,
          settings = {
            Lua = {
              -- make the language server recognize "vim" global
              diagnostics = {
                globals = { 'vim' },
              },
              workspace = {
                -- make language server aware of runtime files
                library = {
                  [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                  [vim.fn.stdpath('config') .. '/lua'] = true,
                },
              },
            },
          },
        })
      end,
    })
  end,
}
