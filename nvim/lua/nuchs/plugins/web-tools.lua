return {
	"ray-x/web-tools.nvim",
	keys = {
		{ "<Leader>bs", "<Cmd>BrowserSync<CR>", desc = "Start Browser-Sync" },
		{ "<Leader>bo", "<Cmd>BrowserOpen<CR>", desc = "Open Browser-Sync" },
		{ "<Leader>bv", "<Cmd>BrowserPreview<CR>", desc = "Preview Browser-Sync" },
		{ "<Leader>br", "<Cmd>BrowserRestart<CR>", desc = "Restart Browser-Sync" },
		{ "<Leader>bp", "<Cmd>BrowserStop<CR>", desc = "Stop Browser-Sync" },
		{ "<Leader>bh", "<Cmd>HurlRun<CR>", desc = "Hurl" },
	},
	config = function()
		require("web-tools").setup()
	end,
}
