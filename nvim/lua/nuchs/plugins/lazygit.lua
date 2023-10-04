return {
	"kdheepak/lazygit.nvim",
	-- optional for floating window border decoration
	dependencies = {
		"nvim-lua/plenary.nvim",
	},
	keys = {
		{ "<Leader>v", "<Cmd>LazyGit<CR>", desc = "Open LazyGit" },
	},
}
