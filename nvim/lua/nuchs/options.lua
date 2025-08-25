-- Set up icons for diagnostics
vim.diagnostic.config({
  signs = {
    text = {
      [vim.diagnostic.severity.ERROR] = " ",
      [vim.diagnostic.severity.WARN]  = " ",
      [vim.diagnostic.severity.HINT]  = "󰠠 ",
      [vim.diagnostic.severity.INFO]  = " ",
    },
  },
})

-- Make sure we use the correct filetype for some ambiguous names
local utils = require('nuchs.utils')
utils.set_filetype({ 'docker-compose.yml' }, 'yaml.docker-compose')

-- General Options
local opt = vim.opt
opt.backup = false
opt.clipboard = 'unnamedplus'
opt.colorcolumn = '+1,+2'
opt.completeopt = { 'menu', 'menuone', 'noselect' } -- do I actually want noselect?
opt.conceallevel = 0
opt.cursorline = true
opt.diffopt:append('iwhite')
opt.diffopt:append('vertical')
opt.expandtab = true
opt.fileencoding = 'utf-8'
opt.foldmethod = 'marker'
opt.hlsearch = true
opt.ignorecase = true
opt.iskeyword:append('-')
opt.linebreak = true
opt.mouse = 'a'
opt.number = true
opt.numberwidth = 4
opt.pumheight = 10
opt.relativenumber = false
opt.runtimepath:remove('/usr/share/vim/vimfiles')
opt.scrolloff = 5
opt.shiftwidth = 2
opt.shortmess:append('c')
opt.showmatch = true
opt.showtabline = 0
opt.sidescrolloff = 5
opt.signcolumn = 'yes'
opt.smartcase = true
opt.smartindent = true
opt.smarttab = true
opt.splitbelow = true
opt.splitright = true
opt.swapfile = false
opt.tabstop = 2
opt.termguicolors = true
opt.textwidth = 80
opt.timeoutlen = 500
opt.undofile = true
opt.updatetime = 250
opt.viewoptions:remove('curdir')
opt.wrap = true
opt.writebackup = false
opt.shada:append('f1')
