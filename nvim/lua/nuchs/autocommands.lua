vim.cmd [[
augroup common_buffer
    autocmd!
    autocmd BufWinEnter * silent! loadview
    autocmd BufWinLeave * silent! mkview
    autocmd BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup end
]]
