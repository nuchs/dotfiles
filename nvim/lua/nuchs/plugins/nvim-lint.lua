return {
  'mfussenegger/nvim-lint',
  event = { 'BufWritePre' },
  config = function()
    local lint = require('lint')

    lint.linters_by_ft = {
      makefile = { 'checkmake' },
    }

    vim.api.nvim_create_autocmd({ 'BufWritePost' }, {
      callback = function()
        lint.try_lint()
        lint.try_lint('codespell')
      end,
    })
  end,
}
