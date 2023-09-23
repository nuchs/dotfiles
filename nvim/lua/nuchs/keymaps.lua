local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- Space for leader
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Quicker escaping from insert mode
keymap("i", "jk", "<Esc>", opts)
keymap("i", "kj", "<ESC>", opts)

-- Manage buffers
keymap("n", "<Leader>e", ":Lex 30<CR>", opts)
keymap("n", "<A-h>", ":bnext<CR>", opts)
keymap("n", "<A-l>", ":bprev<CR>", opts)
keymap("n", "<Leader><Space>", ":b#<CR>", opts)
keymap("n", "<Leader>q", ":q<CR>", opts)
keymap("n", "<Leader>Q", ":wqa<CR>", opts)

-- Quick edits to nvim config
keymap("n", "<Leader>v", ":e $MYVIMRC<CR>", opts)
keymap("n", "<Leader>sv", ":source $MYVIMRC<CR>", opts)

-- Move text about
keymap("v", "<", "<gv^", opts)
keymap("v", ">", ">gv^", opts)
keymap("v", "<A-j>", ":m '>+1<CR>gv=gv", opts)
keymap("v", "<A-k>", ":m '<-2<CR>gv=gv", opts)

-- Don't yank deleted text, it's annoying
keymap("v", "p", '"_dP', opts)

