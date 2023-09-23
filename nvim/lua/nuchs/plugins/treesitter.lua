return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  dependencies = { 
    "nvim-tree/nvim-web-devicons",
    "windwp/nvim-ts-autotag",
    "andymass/vim-matchup",
    "luckasRanarison/tree-sitter-hypr",
  }, 
  config = function () 
    local configs = require("nvim-treesitter.configs")

    configs.setup({
      ensure_installed = { 
        "bash",
        "c",
        "c_sharp",
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
        "toml",
        "typescript",
        "vim",
        "vimdoc",
        "yaml",
      },
      matchup = {
        enable = true,
      },
      autotag = { enable = true },
      sync_install = false,
      highlight = { enable = true },
      indent = { enable = true },  
    })

    local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
    parser_config.hypr = {
      install_info = {
        url = "https://github.com/luckasRanarison/tree-sitter-hypr",
        files = { "src/parser.c" },
        branch = "master",
      },
      filetype = "hypr",
    }
  end
}
