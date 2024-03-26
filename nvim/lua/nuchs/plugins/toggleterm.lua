return {
  'akinsho/toggleterm.nvim',
  version = "*",
  config = function()
    require("toggleterm").setup({
      direction = "vertical",
      size = 80,
      start_in_insert = true,
    })

    local opts = { noremap = true, silent = true }
    vim.keymap.set('t', 'jk', [[<C-\><C-n>]], opts)
    vim.keymap.set('t', '<C-\\>', '<CMD>exe v:count1 . "ToggleTerm name=\\"term" . v:count1 . "\\""<CR>', opts)
    vim.keymap.set('', '<C-\\>', '<CMD>exe v:count1 . "ToggleTerm name=\\"term" . v:count1 . "\\""<CR>', opts)

    -- Go specific terminal mappings, probably need a better place for this
    vim.keymap.set('n', '<Leader>gb', '<CMD>exe v:count1 . "TermExec cmd=\\"go build .\\" name=\\"term1\\""<CR>', opts)
    vim.keymap.set('n', '<Leader>gr', '<CMD>exe v:count1 . "TermExec cmd=\\"go run .\\" name=\\"term1\\""<CR>', opts)
    vim.keymap.set('n', '<Leader>gt', '<CMD>exe v:count1 . "TermExec cmd=\\"go test ./...\\" name=\\"term1\\""<CR>', opts)
  end
}
