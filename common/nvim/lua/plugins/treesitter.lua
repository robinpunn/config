return {
  "nvim-treesitter/nvim-treesitter",
  build = ":TSUpdate",
  config = function()
    require('nvim-treesitter').setup {
      ensure_installed = {
        "python",
        "html",
        "javascript",
        "typescript",
        "tsx",
	      "go",
	      "solidity",
	      "rust",
	      "c",
	      "asm",
	      "commonlisp",
      },
      highlight = { enable = true },
      indent = { enable = true },
      fold = { enable = true },
    }
  end,
}

