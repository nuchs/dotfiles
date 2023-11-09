return {
  "rcarriga/nvim-notify",
  lazy = true,
  config = function()
    require("notify").setup()
  end,
}
