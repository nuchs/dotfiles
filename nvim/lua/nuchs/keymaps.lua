local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }
local utils = require('nuchs.utils')

-- Space for leader
keymap('', '<Space>', '<Nop>', opts)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Clear search highlighting
keymap(
  'n',
  '<Leader>cc',
  ':noh<CR>',
  { noremap = true, silent = true, desc = 'Clear highlighting' }
)

-- Quicker escaping from insert mode
keymap('i', 'jk', '<Esc>', opts)
keymap('i', 'kj', '<ESC>', opts)

-- Manage buffers and windows
keymap(
  'n',
  '<Leader><Space>',
  ':b#<CR>',
  { noremap = true, silent = true, desc = 'Switch to last buffer' }
)
keymap('n', '<C-q>', ':qa!<CR>', { noremap = true, silent = true, desc = 'Quit' })
keymap('n', '<Leader>q', ':q<CR>', { noremap = true, silent = true, desc = 'Close Window' })
keymap(
  'n',
  '<Leader>Q',
  ':b#<BAR>bd #<CR>',
  { noremap = true, silent = true, desc = 'Kill buffer' }
)
keymap('n', '<Leader>w', ':w<CR>', { noremap = true, silent = true, desc = 'Save buffer' })
keymap('n', '<Leader>W', ':wa<CR>', { noremap = true, silent = true, desc = 'Save all buffers' })
keymap('n', '<Tab>', ':bn<CR>', { noremap = true, silent = true, desc = 'Next buffer' })
keymap('n', '<S-Tab>', ':bp<CR>', { noremap = true, silent = true, desc = 'Previous buffer' })

-- Move text about
keymap('v', '<A-h>', '<gv^', opts)
keymap('v', '<A-l>', '>gv^', opts)
keymap('v', '<A-j>', ":m '>+1<CR>gv=gv", opts)
keymap('v', '<A-k>', ":m '<-2<CR>gv=gv", opts)

keymap('n', '<A-h>', '<<', opts)
keymap('n', '<A-l>', '>>', opts)
keymap('n', '<A-j>', ':m .+1<CR>==', opts)
keymap('n', '<A-k>', ':m .-2<CR>==', opts)

keymap('i', '<A-j>', '<Esc>:m .+1<CR>==gi', opts)
keymap('i', '<A-k>', '<Esc>:m .-2<CR>==gi', opts)

-- Don't yank deleted text, it's annoying
keymap('v', 'p', '"_dP', opts)

-- Move out of paired delimiters e.g. brackets or quotes
vim.keymap.set('i', '<C-e>', utils.escape_pair, { noremap = true, silent = true })

-- Show the messages buffer
vim.keymap.set('n', '<Leader>vm', ':messages<CR>', { noremap = true, silent = true })

-- Change the cwd to the containing folder for the current buffer
vim.keymap.set('n', '<Leader>cd', ':cd %:h<CR>')
