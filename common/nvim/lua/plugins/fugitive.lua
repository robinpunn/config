return {
  "tpope/vim-fugitive",
  cmd = { "G" },  -- lazy-load on :Git command
  keys = {
    { "<leader>gs", "<cmd>G<cr>", desc = "Git status" },
  },
}

