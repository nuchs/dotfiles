local function dap_toggle_breakpoint()
	require("dap").toggle_breakpoint()
end

local function dap_continue()
	require("dap").continue()
end

local function setup_go_debugger()
	require("dap-go").setup()
end

local function setup_js_debugger(dap)
	require("dap-vscode-js").setup({
		debugger_path = vim.fn.stdpath("data") .. "/lazy/vscode-js-debug",
		adapters = { "pwa-node", "pwa-chrome" },
	})

	for _, language in ipairs({ "typescript", "javascript" }) do
		dap.configurations[language] = {
			{
				type = "pwa-node",
				request = "launch",
				name = "1. Launch file in node",
				program = "${file}",
				cwd = "${workspaceFolder}",
			},
			{
				type = "pwa-chrome",
				name = "2. Launch file in Chrome",
				request = "launch",
				url = "http://localhost:5173",
				sourceMaps = true,
				protocol = "inspector",
				port = 9222,
				webRoot = "${workspaceFolder}/src",
				skipFiles = { "**/node_modules/**/*", "**/@vite/*", "**/src/client/*", "**/src/*" },
			},
			{
				type = "pwa-node",
				request = "attach",
				processId = require("dap.utils").pick_process,
				name = "3. Attach to node process",
				cwd = "${workspaceFolder}/src",
			},
		}
	end
end

return {
	"mfussenegger/nvim-dap",
	dependencies = {
		"rcarriga/nvim-dap-ui",
		"mxsdev/nvim-dap-vscode-js",
		"leoluz/nvim-dap-go",
		{
			"microsoft/vscode-js-debug",
			version = "1.x",
			build = "npm i && npm run compile vsDebugServerBundle && mv dist out",
		},
	},
	keys = {
		{ "<F5>", dap_continue },
		{ "<Leader>db", dap_toggle_breakpoint },
	},
	config = function()
		local dap = require("dap")
		vim.keymap.set("n", "dt", dap.repl.toggle)
		vim.keymap.set("n", "<F10>", dap.step_over)
		vim.keymap.set("n", "<F11>", dap.step_into)
		vim.keymap.set("n", "<F12>", dap.step_out)

		setup_js_debugger(dap)
		setup_go_debugger()

		local dapui = require("dapui")
		dapui.setup()

		dap.listeners.after.event_initialized["dapui_config"] = function()
			dapui.open()
		end
		dap.listeners.before.event_terminated["dapui_config"] = function()
			dapui.close()
		end
		dap.listeners.before.event_exited["dapui_config"] = function()
			dapui.close()
		end
	end,
}
