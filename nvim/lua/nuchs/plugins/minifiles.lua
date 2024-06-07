return {
	"echasnovski/mini.files",
	version = false,
	lazy = true,
	keys = {
		{ "<Leader>e", "<Cmd>lua MiniFiles.open()<CR>", desc = "Open file explorer" },
	},
	dependencies = {
		"nuchs/neovim-session-manager",
	},
	config = function()
		local mini = require("mini.files")
		mini.setup({
			options = {
				use_as_default_explorer = true,
			},
			windows = {
				preview = true,
				width_preview = 80,
			},
		})

		vim.cmd("highlight MiniFilesTitleFocused guifg=#FFD700")

		local files_set_cwd = function()
			local cur_entry_path = mini.get_fs_entry().path
			local cur_directory = vim.fs.dirname(cur_entry_path)
			vim.fn.chdir(cur_directory)
			require("session_manager").load_current_dir_session()
		end

		vim.api.nvim_create_autocmd("User", {
			pattern = "MiniFilesBufferCreate",
			callback = function(args)
				vim.keymap.set("n", "<Home>", files_set_cwd, { buffer = args.data.buf_id })
			end,
		})
	end,
}
