return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  ft = {
    "c",
    "cpp",
    "cs",
    "css",
    "csv",
    "dockerfile",
    "go",
    "htm",
    "html",
    "js",
    "json",
    "jsx",
    "lua",
    "makefile",
    "md",
    "mod",
    "nvim",
    "py",
    "sh",
    "sql",
    "sum",
    "tf",
    "toml",
    "ts",
    "tsx",
    "vim",
    "xml",
    "yaml",
  },
  lazy = true,
  config = function()
    local configs = require("nvim-treesitter.configs")

    configs.setup({
      ensure_installed = {
        "bash",
        "c",
        "c_sharp",
        "comment",
        "cpp",
        "css",
        "csv",
        "dockerfile",
        "git_config",
        "git_rebase",
        "go",
        "gomod",
        "gosum",
        "helm",
        "html",
        "javascript",
        "json",
        "lua",
        "make",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "rust",
        "sql",
        "terraform",
        "toml",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "xml",
        "yaml",
      },
      sync_install = false,
      highlight = { enable = true },
      indent = { enable = true },
    })
  end,
}
