-- helper to jump and show the diagnostic under cursor
local function jump_and_show(delta)
  vim.diagnostic.jump({ count = delta })
  vim.schedule(function()
    vim.diagnostic.open_float(nil, {
      focus = false,
      scope = 'cursor',
      border = 'rounded',
      source = 'if_many',
    })
  end)
end

return {
  'neovim/nvim-lspconfig',
  event = { 'BufReadPre', 'BufNewFile' },
  dependencies = {
    'hrsh7th/cmp-nvim-lsp',
    'nvim-telescope/telescope.nvim',
  },
  config = function()
    -- Keymaps for every attached client
    vim.api.nvim_create_autocmd('LspAttach', {
      group = vim.api.nvim_create_augroup('my.lsp.attach', {}),
      callback = function(args)
        local bufnr = args.buf
        local map = function(mode, lhs, rhs, desc)
          vim.keymap.set(
            mode,
            lhs,
            rhs,
            { buffer = bufnr, silent = true, noremap = true, desc = desc }
          )
        end
        map('n', 'gr', '<cmd>Telescope lsp_references<CR>', 'LSP references')
        map('n', 'gD', vim.lsp.buf.declaration, 'Go to declaration')
        map('n', 'gd', '<cmd>Telescope lsp_definitions<CR>', 'Go to definition')
        map('n', 'gi', '<cmd>Telescope lsp_implementations<CR>', 'Go to implementation')
        map('n', 'gt', '<cmd>Telescope lsp_type_definitions<CR>', 'Type definitions')
        map({ 'n', 'v' }, '<Leader>a', vim.lsp.buf.code_action, 'Code action')
        map('n', '<F2>', vim.lsp.buf.rename, 'Rename')
        map('n', '[[', function()
          jump_and_show(-1)
        end, 'Prev diagnostic')
        map('n', ']]', function()
          jump_and_show(1)
        end, 'Next diagnostic')
        map('n', 'K', vim.lsp.buf.hover, 'Hover')
      end,
    })

    local capabilities = require('cmp_nvim_lsp').default_capabilities()
    vim.lsp.config('*', { capabilities = capabilities })
    vim.lsp.config('lua_ls', {
      capabilities = capabilities,
      settings = {
        Lua = {
          diagnostics = { globals = { 'vim' } },
          workspace = {
            library = {
              [vim.fn.expand('$VIMRUNTIME/lua')] = true,
              [vim.fn.stdpath('config') .. '/lua'] = true,
            },
          },
        },
      },
    })
  end,
}
