return {
	"folke/noice.nvim",
	event = "VeryLazy",
	opts = {
		presets = {
			command_palette = true,
		},
		routes = {
			{
				filter = {
					event = "msg_show",
					kind = "",
					find = "written",
				},
				opts = { skip = true },
			},
		},
		lsp = {
			progress = { enabled = true },
		},
	},
	dependencies = {
		"MunifTanjim/nui.nvim",
		"rcarriga/nvim-notify",
		"nvim-treesitter/nvim-treesitter",
	},
}
