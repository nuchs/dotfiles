vim.cmd([[
augroup nuchs_save_folds
 autocmd!
 autocmd BufWinEnter * silent! loadview
 autocmd BufWinLeave * silent! mkview
augroup end
]])

vim.cmd([[
augroup nuchs_save_cursor_position
 autocmd!
 autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup end
]])

vim.cmd([[
augroup nuchs_formatting
  autocmd!
  autocmd BufWritePre *.go\|*.lua\|*.js\|*.html\|*.css lua vim.lsp.buf.format()
augroup end
]])
