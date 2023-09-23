return {
  "williamboman/mason.nvim",
  dependencies = { "williamboman/mason-lspconfig.nvim" },
  priority = 100, -- to ensure this loads before lspconfig
  config = function()
    local mason = require("mason")
    local mason_lspconfig = require("mason-lspconfig")

    mason.setup({
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗"
        }
      }
    })

    mason_lspconfig.setup({

      ensure_installed = {
        "bashls",
        "cssls",
        "cssmodules_ls",
        "diagnosticls",
        "docker_compose_language_service",
        "dockerls",
        "emmet_language_server",
        "eslint",
        "html",
        "jqls",
        "jsonls",
        "lua_ls",
        "omnisharp",
        "pyright",
        "rust_analyzer",
        "sqls",
        "tsserver",
        "vimls",
        "yamlls",
      },
    })
  end
}

