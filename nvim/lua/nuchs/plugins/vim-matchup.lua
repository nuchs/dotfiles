return {
	"andymass/vim-matchup",
	lazy = true,
	config = function()
		vim.g.matchup_matchparen_offscreen = { method = "popup" }
	end,
}
