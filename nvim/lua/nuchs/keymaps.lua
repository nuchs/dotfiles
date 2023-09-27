local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- Space for leader
keymap("", "<Space>", "<Nop>", opts)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Clear search highlighting
keymap("n", "<Leader>c", ":noh<CR>", { noremap = true, silent = true, desc = "Clear highlighting" })

-- Quicker escaping from insert mode
keymap("i", "jk", "<Esc>", opts)
keymap("i", "kj", "<ESC>", opts)

-- Manage buffers
keymap("n", "<A-h>", ":bnext<CR>", opts)
keymap("n", "<A-l>", ":bprev<CR>", opts)
keymap("n", "<Leader><Space>", ":b#<CR>", { noremap = true, silent = true, desc = "Switch to last buffer" })
keymap("n", "<Leader>q", ":q<CR>", { noremap = true, silent = true, desc = "Kill buffer" })
keymap("n", "<Leader>Q", ":wqa<CR>", { noremap = true, silent = true, desc = "Save and kill all buffers" })

-- Move text about
keymap("v", "<", "<gv^", opts)
keymap("v", ">", ">gv^", opts)
keymap("v", "<A-j>", ":m '>+1<CR>gv=gv", opts)
keymap("v", "<A-k>", ":m '<-2<CR>gv=gv", opts)

-- Don't yank deleted text, it's annoying
keymap("v", "p", '"_dP', opts)

