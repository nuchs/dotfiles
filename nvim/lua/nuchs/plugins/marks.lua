return {
  "chentoast/marks.nvim",
  config = function()
    require("marks").setup({
      refresh_interval = 250,
      force_write_shada = true,
    })
  end,
}
