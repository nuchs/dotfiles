-- keymap helpers --------------------------------------------------------------
local base = { noremap = true, silent = true }

local function merge_opts(extra)
  return vim.tbl_extend('force', base, extra or {})
end

local function map(mode, lhs, rhs, o)
  local opts = type(o) == 'string' and { desc = o } or (o or {})
  vim.keymap.set(mode, lhs, rhs, merge_opts(opts))
end

local function nmap(lhs, rhs, o)
  map('n', lhs, rhs, o)
end
local function imap(lhs, rhs, o)
  map('i', lhs, rhs, o)
end
local function vmap(lhs, rhs, o)
  map('v', lhs, rhs, o)
end

-- Space for leader
map('', '<Space>', '<Nop>')
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Clear annoyances
nmap('<Leader>cc', ':noh<CR>', 'Clear highlighting')
nmap('<Leader>cr', ':%s/\\r//<CR>', 'Clear windows line endings')

-- Quicker escaping from insert mode
imap('jk', '<Esc>')

-- Easier navigation across long lines
nmap('j', 'gj')
nmap('gj', 'j')
nmap('k', 'gk')
nmap('gk', 'k')

-- Manage buffers and windows
nmap('<Leader><Space>', ':b#<CR>', 'Switch to last buffer')
nmap('<C-q>', ':qa!<CR>', 'Quit')
nmap('<Leader>q', ':q<CR>', 'Close Window')
nmap('<Leader>Q', ':b#<BAR>bd #<CR>', 'Kill buffer')
nmap('<Leader>w', ':w<CR>', 'Save buffer')
nmap('<Leader>W', ':wa<CR>', 'Save all buffers')
nmap('<Tab>', ':bn<CR>', 'Next buffer')
nmap('<S-Tab>', ':bp<CR>', 'Previous buffer')

-- Move text about
vmap('<A-h>', '<gv^')
vmap('<A-l>', '>gv^')
vmap('<A-j>', ":m '>+1<CR>gv=gv")
vmap('<A-k>', ":m '<-2<CR>gv=gv")

nmap('<A-h>', '<<')
nmap('<A-l>', '>>')
nmap('<A-j>', ':m .+1<CR>==')
nmap('<A-k>', ':m .-2<CR>==')

imap('<A-j>', '<Esc>:m .+1<CR>==gi')
imap('<A-k>', '<Esc>:m .-2<CR>==gi')

-- Don't yank text when you paste over it, it's annoying
vmap('p', '"_dP')

-- Show the messages buffer
nmap('<Leader>vm', ':messages<CR>')

-- Change the cwd to the containing folder for the current buffer
nmap('<Leader>cd', ':cd %:h<CR>')

-- Easier navigation of quickfix list
nmap('<C-n>', ':cnext<CR>', 'Next quickfix item')
nmap('<C-p>', ':cprev<CR>', 'Prev quickfix item')
