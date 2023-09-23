return { 
  'rcarriga/nvim-notify',
  lazy = true,
  config = function()
    require("notify").setup({
      background_colour = "#1d2021",
    })
  end
}
