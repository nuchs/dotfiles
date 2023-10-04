return {
	"pappasam/nvim-repl",
	init = function()
		vim.g["repl_filetype_commands"] = {
			javascript = "node",
			lua = "lua",
		}
	end,
	keys = {
		{ "<leader>rt", "<cmd>ReplToggle<cr>", desc = "Toggle nvim-repl" },
		{ "<leader>rc", "<cmd>ReplRunCell<cr>", desc = "nvim-repl run cell" },
	},
	config = function()
		local opts = { noremap = true, silent = true }
		vim.keymap.set("n", "rr", "<Plug>ReplSendLine", opts)
		vim.keymap.set("v", "rr", "<Plug>ReplSendVisual", opts)
	end,
}
