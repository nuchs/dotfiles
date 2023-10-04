return {
	"mfussenegger/nvim-lint",
	event = "BufWritePre",
	config = function()
		local lint = require("lint")

		lint.linters_by_ft = {
			proto = { "buf_lint" },
			javascript = { "eslint_d" },
			typescript = { "eslint_d" },
			markdown = { "markdown_lint" },
			yaml = { "yaml_lint" },
			json = { "json_lint" },
		}

		vim.api.nvim_create_autocmd({ "BufWritePost" }, {
			callback = function()
				lint.try_lint()
			end,
		})
	end,
}
