local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'

if not vim.loop.fs_stat(lazypath) then
  print('Installing lazy.nvim....')
  vim.fn.system({
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    path,
  })
end

vim.opt.rtp:prepend(lazypath)

require('lazy').setup('nuchs/plugins', { 
  ui = { border = "rounded" },
  dev = { path = "~/.work" },
  checker = { 
    enabled = true,
    frequency = 7200,
  },
  performance = {
    disabled_plugins = {
      "gzip",
      "tarPlugin",
      "tutor",
      "zipPlugin",
    }
  }
})
