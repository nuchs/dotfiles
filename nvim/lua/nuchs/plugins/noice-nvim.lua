return {
  "folke/noice.nvim",
  event = "VeryLazy",
  opts = {
    presets = {
      command_palette = true
    },
    routes = {
      view = "notify",
      filter = { event = "msg_showmode" },
    },
    lsp = {
      progress = { enabled = true, }
    }
  },
  dependencies = {
    "MunifTanjim/nui.nvim",
    "rcarriga/nvim-notify",
    "nvim-treesitter/nvim-treesitter",
  }
}
