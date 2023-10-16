return {
	'echasnovski/mini.starter',
	version = false,
	dependencies = { 'echasnovski/mini.sessions' },
	config = function()
	  local starter = require('mini.starter')
	  starter.setup({
	    items = {
	      starter.sections.sessions(5, true),
	      starter.sections.recent_files(5, false),
	      starter.sections.builtin_actions(),
	    },
	  })
	end
}
