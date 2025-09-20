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

vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter", "CursorHold", "CursorHoldI" }, {
  command = "checktime",
})

vim.api.nvim_create_autocmd("FileChangedShellPost", {
  callback = function(args)
    vim.notify(
      ("File changed on disk. Reloaded: %s"):format(vim.api.nvim_buf_get_name(args.buf)),
      vim.log.levels.INFO
    )
  end,
})
