return {
	"nvim-lualine/lualine.nvim",
	dependencies = {
		"nvim-tree/nvim-web-devicons",
		"echasnovski/mini.sessions",
	},
	config = function()
		local sessions = require("mini.sessions")

		require("lualine").setup({
			options = {
				theme = "tokyonight",
			},
			sections = {
				lualine_c = { sessions.get_latest, { "filename", path = 1 } },
				lualine_y = { "progress", "location" },
				lualine_z = { { "datetime", style = "%H:%M" } },
			},
		})
	end,
}
