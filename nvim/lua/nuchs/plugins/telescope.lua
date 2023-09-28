return {
	"nvim-telescope/telescope.nvim",
	tag = "0.1.3",
	keys = {
		{ "<Leader>t", "<Cmd>Telescope<CR>", desc = "Open Telescope" },
		{ "<Leader>ff", "<Cmd>Telescope fd<CR>", desc = "Find files in cwd" },
		{ "<Leader>fr", "<Cmd>Telescope oldfiles<CR>", desc = "Mru files" },
		{ "<Leader>fg", "<Cmd>Telescope live_grep<CR>", desc = "Search in cwd" },
		{ "<Leader>fb", "<Cmd>Telescope buffers<CR>", desc = "Select buffer" },
		{ "<Leader>h", "<Cmd>Telescope help_tags<CR>", desc = "Search help" },
		{ "<Leader>z", "<Cmd>Telescope zoxide list<CR>", desc = "Change directory" },
		{ "<Leader>k", "<Cmd>Telescope keymaps<CR>", desc = "List keymaps" },
		{ "<Leader>p", "<Cmd>Telescope luasnip<CR>", desc = "Search snippets" },
	},
	dependencies = {
		"nvim-lua/plenary.nvim",
		"jvgrootveld/telescope-zoxide",
		"benfowler/telescope-luasnip.nvim",
	},
	config = function()
		local telescope = require("telescope")
		telescope.setup({
			pickers = {
				buffers = {
					mappings = {
						n = {
							["<C-d>"] = require("telescope.actions").delete_buffer,
						},
						i = {
							["<C-d>"] = require("telescope.actions").delete_buffer,
						},
					},
				},
			},
		})
		telescope.load_extension("zoxide")
		telescope.load_extension("luasnip")
	end,
}
