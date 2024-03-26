return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  ft = {
    "htm",
    "html",
    "md",
    "xml",
    "sh",
    "dockerfile",
    "go",
    "csv",
    "css",
    "js",
    "json",
    "lua",
    "md",
    "py",
    "sql",
    "toml",
    "nvim",
    "vim",
    "yaml",
  },
  lazy = true,
  config = function()
    local configs = require("nvim-treesitter.configs")

    configs.setup({
      ensure_installed = {
        "bash",
        "css",
        "csv",
        "dockerfile",
        "go",
        "html",
        "javascript",
        "json",
        "lua",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "rust",
        "sql",
        "vim",
        "vimdoc",
        "yaml",
      },
      sync_install = false,
      highlight = { enable = true },
      indent = { enable = true },
    })
  end,
}
