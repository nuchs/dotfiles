return {
	"Exafunction/codeium.nvim",
	lazy = true,
	event = "InsertEnter",
	keys = {
		{ "<Leader>c", "<Cmd>Codeium Chat<CR>", desc = "Chat to codeium" },
	},
	dependencies = {
		"nvim-lua/plenary.nvim",
		"hrsh7th/nvim-cmp",
	},
	config = function()
		require("codeium").setup({
			enable_chat = true,
		})
	end,
}
