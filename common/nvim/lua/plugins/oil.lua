return {
  'stevearc/oil.nvim',
  opts = {
     default_file_explorer = false,	
  },
  dependencies = { "nvim-tree/nvim-web-devicons" },
  lazy = false,
  keys = {
    { "<leader>o", function() require("oil").open_float() end, desc = "Open Oil (float) in current directory" },
    { "<leader>O", function() require("oil").open_float(vim.fn.getcwd()) end, desc = "Open Oil (float) in project root"},
  }
}

