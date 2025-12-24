return {
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',  
  dependencies = {
    'nvim-lua/plenary.nvim',
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build = 'make',
      cond = function() return vim.fn.executable 'make' == 1 end,
    },
  },
  opts = {
    extensions = {
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      }
    }
  },
  config = function()
    -- Load FZF 
    pcall(function() require('telescope').load_extension('fzf') end)
  end,
  keys = {
    { '<leader>ff', function() require('telescope.builtin').find_files() end, desc = 'Telescope find files' },
    { '<leader>fg', function() require('telescope.builtin').live_grep() end, desc = 'Telescope live grep' },
    { '<leader>fs', function() require('telescope.builtin').grep_string() end, desc = 'Telescope grep string under cursor' },
    { '<leader>fb', function() require('telescope.builtin').buffers() end, desc = 'Telescope buffers' },
    { '<leader>fh', function() require('telescope.builtin').help_tags() end, desc = 'Telescope help tags' },
    { '<leader>fo', function() require('telescope.builtin').oldfiles() end, desc = 'Find old files' },
  },
}

