-- Plumbing to setup the lazy plugin manager {{{1
local lazy = {}
lazy.path = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
lazy.opts = { 
  ui = { border = "rounded" },
  dev = { path = "~/.work" },
}

function lazy.install(path)
  if not vim.loop.fs_stat(path) then
    print('Installing lazy.nvim....')
    vim.fn.system({
      'git',
      'clone',
      '--filter=blob:none',
      'https://github.com/folke/lazy.nvim.git',
      '--branch=stable', -- latest stable release
      path,
    })
  end
end

function lazy.setup(plugins)
  -- Uncomment to reinstall lazy plugin manager
  -- lazy.install(lazy.path)

  vim.opt.rtp:prepend(lazy.path)
  require('lazy').setup(plugins, lazy.opts)
end

-- Load Plugins {{{1
lazy.setup({

  -- Vim-hypr-nav: Navigate between split and hyprv windows {{{2
  { "nuchs/vim-hypr-nav" },

  -- Plenary: utility functions for other plugins {{{2
  { 
    "nvim-lua/plenary.nvim",
    lazy = true,
  },

  -- Colorscheme: Gruvbox {{{2
  {
    "ellisonleao/gruvbox.nvim", 
    priority = 1000,
    config = function()
      vim.cmd.colorscheme('gruvbox')
      vim.cmd[[highlight Normal guibg=none]]
    end,
  },

  -- Nvim web dev icons {{{2
  { 
    "nvim-tree/nvim-web-devicons", 
    lazy = true 
  },
  

  -- Nui: nvim widgets {{{2
  { 
    'MunifTanjim/nui.nvim',
    lazy = true,
  },

  -- Nvim-notify: popup notifications {{{2
  { 
    'rcarriga/nvim-notify',
    lazy = true,
    config = function()
      require("notify").setup({
        background_colour = "#1d2021",
      })
    end
  },

  -- Lualine: status line -- {{{2
  { 
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'echasnovski/mini.sessions', 
    },
    config = function() 
      local sessions = require("mini.sessions")

      require('lualine').setup({
        sections = {
          lualine_c = {sessions.get_latest, "filename"}
        }
      })
    end
  },

  -- Nvim surround: motions to add paired characters {{{2
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
      })
    end
  },

  -- Noice: command line in center and notifications {{{2
  {
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
  },

  -- mini.session: Session management {{{2
  { 
    'echasnovski/mini.sessions', 
    version = false ,
    config = function()
      require('mini.sessions').setup()
    end
  },

  -- mini.starter: Start screen {{{2
  { 
    'echasnovski/mini.starter', 
    version = '*',
    dependencies = { 'echasnovski/mini.sessions' },
    config = function()
      local starter = require('mini.starter')
      starter.setup({
        items = {
          starter.sections.sessions(5, true),
          starter.sections.recent_files(5, true),
          starter.sections.recent_files(5, false),
          starter.sections.builtin_actions(),
        },
      })
    end
  },

  -- Tree sitter: better highlighting {{{2

  -- Tree-sitter-hypr: Syntax highlighting for hyprland config file 
  { "luckasRanarison/tree-sitter-hypr" },

  -- vim-matchup
  {
    "andymass/vim-matchup",
    lazy = true,
    config = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
    end,
  },

  -- Main plugin
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    dependencies = { 
      "nvim-tree/nvim-web-devicons",
      "windwp/nvim-ts-autotag",
      "andymass/vim-matchup",
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
  },

  -- Telescope: Fuzzy finder {{{2

  -- zoxide extension
  {
    "jvgrootveld/telescope-zoxide",
    lazy = true,
  },

  -- LuaSnip extension
  {
    "benfowler/telescope-luasnip.nvim",
    lazy = true,
  },

  -- basic ui extension
  {
    'nvim-telescope/telescope-ui-select.nvim',
    lazy = true,
  },

  -- Telescope plugin
  {
    'nvim-telescope/telescope.nvim', 
    tag = '0.1.3',
    dependencies = { 
      'nvim-lua/plenary.nvim' ,
      'jvgrootveld/telescope-zoxide',
      'benfowler/telescope-luasnip.nvim',
    },
    config = function ()
      local telescope = require('telescope')
      telescope.setup()
      telescope.load_extension('zoxide')
      telescope.load_extension('luasnip')
      telescope.load_extension('ui-select')
    end,
  },

  -- Comment toggling {{{2
  { 
    'numToStr/Comment.nvim', 
    opts = {},
    event = "InsertEnter",
  },

  -- Nvim-autopairs {{{2
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    opts = {} 
  },

  -- nvim-ts-autotag: Auto close/update html tags {{{2
  {
    'windwp/nvim-ts-autotag',
    filetype = { "htm", "html", "jsx", "md", "tsx", "xml" },
    lazy = true,
  },

  -- Flash: navigate around the screen faster {{{2
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      { "s", mode = { "n", "o", "x" }, function() require("flash").jump() end, desc = "Flash" },
      { "S", mode = { "n", "o", "x" }, function() require("flash").treesitter() end, desc = "Flash Treesitter" },
      { "r", mode = "o", function() require("flash").remote() end, desc = "Remote Flash" },
      { "R", mode = { "o", "x" }, function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
      { "<c-s>", mode = { "c" }, function() require("flash").toggle() end, desc = "Toggle Flash Search" },
    },
  },

  -- Lua snip: snippets {{{2
  { 
    "rafamadriz/friendly-snippets",
    lazy = true,
  },

  -- Snippet Manager
  {
    "L3MON4D3/LuaSnip",
    version = "2.*", 
    build = "make install_jsregexp",
    event = "InsertEnter",
    dependencies = { "rafamadriz/friendly-snippets" },
    config = function()
      require("luasnip.loaders.from_vscode").lazy_load()
    end
  },

  -- LSP
  {
    "williamboman/mason.nvim"
  },

  {
    "williamboman/mason-lspconfig.nvim"
  },

  {
    "neovim/nvim-lspconfig"
  },

  -- Nvim-cmp: auto complete {{{2
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-path",
      "L3MON4D3/cmp-luasnip-choice",
      "L3MON4D3/LuaSnip",
    },
    init = function()

    end,
  },

})


