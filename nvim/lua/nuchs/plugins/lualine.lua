return {
  "nvim-lualine/lualine.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    require("lualine").setup({
      options = {
        theme = "tokyonight",
      },
      sections = {
        lualine_b = { 'branch', 'diagnostics' },
        lualine_c = {
          {
            'filename',
            path = 1
          }
        },
      },
      inactive_sections = {
        lualine_b = { 'diagnostics' },
      }
    })
  end,
}
