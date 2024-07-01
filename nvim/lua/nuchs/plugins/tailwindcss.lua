return {
  'laytan/tailwind-sorter.nvim',
  dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-lua/plenary.nvim' },
  build = 'cd formatter && npm ci && npm run build',
  ft = { 'mjs', 'js', 'ts', 'tsx', 'jsx', 'twig', 'hbs', 'html', 'php', 'heex' },
  config = true,
}
