return {
	"nvim-treesitter/nvim-treesitter",
	build = ":TSUpdate",
	dependencies = {
		"nvim-tree/nvim-web-devicons",
		"windwp/nvim-ts-autotag",
		"andymass/vim-matchup",
	},
	ft = {
		"htm",
		"html",
		"jsx",
		"md",
		"tsx",
		"xml",
		"sh",
		"c",
		"h",
		"cpp",
		"cs",
		"dockerfile",
		"go",
		"csv",
		"css",
		"js",
		"ts",
		"json",
		"lua",
		"md",
		"py",
		"sql",
		"rs",
		"toml",
		"nvim",
		"vim",
		"yaml",
	},
	lazy = true,
	config = function()
		local configs = require("nvim-treesitter.configs")

		configs.setup({
			ensure_installed = {
				"bash",
				"c",
				"c_sharp",
				"css",
				"csv",
				"dockerfile",
				"go",
				"html",
				"javascript",
				"json",
				"lua",
				"markdown",
				"markdown_inline",
				"python",
				"query",
				"regex",
				"rust",
				"sql",
				"toml",
				"typescript",
				"vim",
				"vimdoc",
				"yaml",
			},
			matchup = {
				enable = true,
			},
			autotag = { enable = true },
			sync_install = false,
			highlight = { enable = true },
			indent = { enable = true },
		})
	end,
}
