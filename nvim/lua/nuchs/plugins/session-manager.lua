return {
  "Shatur/neovim-session-manager",
  enabled = true,
  dependencies = {
    "nvim-lua/plenary.nvim",
    "stevearc/dressing.nvim",
  },
  config = function()
    local config = require('session_manager.config')
    local sm = require("session_manager")

    sm.setup({
      autosave_ignore_dirs = { "/", "/home/nuchs" },
      autoload_mode = { config.AutoloadMode.CurrentDir, config.AutoloadMode.LastSession },
    })

    vim.api.nvim_create_autocmd({ 'BufWritePre' }, {
      callback = function()
        -- sm.save_current_session()
      end
    })

    local opts = { noremap = true, silent = true }
    opts.desc = "Save session"
    vim.keymap.set('n', '<Leader>sc', "<Cmd>SessionManager save_current_session<CR>", opts)
    opts.desc = "Load session"
    vim.keymap.set('n', '<Leader>ss', "<Cmd>SessionManager load_session<CR>", opts)
    opts.desc = "Delete session"
    vim.keymap.set('n', '<Leader>sd', "<Cmd>SessionManager delete_session<CR>", opts)

    require("dressing").setup({
      select = {
        telescope = {
          layout_config = {
            width = 80,
            height = 20,
          },
        },
      },
    })
  end
}
