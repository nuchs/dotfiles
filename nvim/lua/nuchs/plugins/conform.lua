return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  config = function()
    require('conform').setup({
      formatters_by_ft = {
        lua = { 'stylua' },
        rust = { 'rustfmt' },
        go = { 'golangci-lint' },
      },
      format_on_save = {
        timeout_ms = 2000,
        lsp_fallback = true,
      },
    })
  end,
}
