local has_words_before = function()
  unpack = unpack or table.unpack
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0
    and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
end

return {
  'hrsh7th/nvim-cmp',
  lazy = true,
  event = 'InsertEnter',
  dependencies = {
    'hrsh7th/cmp-buffer', -- source for text in buffer
    'hrsh7th/cmp-cmdline',
    'hrsh7th/cmp-path', -- source for file system paths
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-nvim-lua',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    'L3MON4D3/LuaSnip', -- snippet engine
    'saadparwaiz1/cmp_luasnip', -- for autocompletion
    'onsails/lspkind.nvim', -- vs-code like pictograms
  },
  config = function()
    local cmp = require('cmp')
    local lspkind = require('lspkind')
    local utils = require('nuchs.utils')
    local luasnip = require('luasnip')
    require('luasnip.loaders.from_vscode').lazy_load()

    cmp.setup({
      completion = {
        completeopt = 'menuone,preview,insert',
      },
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },
      preselect = cmp.PreselectMode.None,
      mapping = cmp.mapping.preset.insert({
        ['<C-c>'] = cmp.mapping.abort(),
        ['<C-e>'] = function()
          if luasnip.expand_or_jumpable() then
            luasnip.expand_or_jump()
          else
            utils.escape_pair()
          end
        end,
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
          elseif has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          else
            fallback()
          end
        end, { 'i', 's' }),
      }),

      sources = cmp.config.sources({
        { name = 'codeium' },
        { name = 'nvim_lsp' },
        { name = 'nvim_lsp_signature_help' },
        { name = 'luasnip' },
        {
          name = 'buffer',
          option = {
            get_bufnrs = function()
              return vim.api.nvim_list_bufs()
            end,
          },
        },
        { name = 'path' },
      }),

      -- configure lspkind for vs-code like pictograms in completion menu
      formatting = {
        format = lspkind.cmp_format({
          maxwidth = 50,
          ellipsis_char = '...',
        }),
      },
    })

    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = 'buffer' },
      },
    })

    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = 'path' },
      }, {
        { name = 'cmdline' },
      }),
    })
  end,
}
