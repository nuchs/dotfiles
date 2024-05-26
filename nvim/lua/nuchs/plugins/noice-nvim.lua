return {
  "folke/noice.nvim",
  event = "VeryLazy",
  opts = {
    presets = {
      command_palette = true,
    },
    lsp = {
      progress = { enabled = true },
    },
  },
  dependencies = {
    "MunifTanjim/nui.nvim",
  },
}
