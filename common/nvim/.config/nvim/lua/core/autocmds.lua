-- folding
vim.api.nvim_create_autocmd("FileType", {
  pattern = "*",
  callback = function()
    vim.wo.foldmethod = "expr"
    vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
    vim.wo.foldlevel = 99  
  end,
})

-- json folding
vim.api.nvim_create_autocmd("FileType", {
  pattern = "json",
  callback = function()
    vim.wo.foldmethod = "indent"
    vim.wo.foldlevel = 1  -- increase this number to unfold more levels
  end,
})

-- line numbers for help
vim.api.nvim_create_autocmd("FileType", {
  pattern = "help",
  callback = function()
    vim.opt_local.number = true
  end,
  group = vim.api.nvim_create_augroup("help_settings", { clear = true }),
})

-- ts/js indentation
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
  callback = function()
    vim.bo.shiftwidth = 2
    vim.bo.tabstop = 2
    vim.bo.softtabstop = 2
    vim.bo.expandtab = true  
  end,
})

-- lua indentation
vim.api.nvim_create_autocmd("FileType", {
  pattern = "lua",
  callback = function()
    vim.bo.shiftwidth = 2
    vim.bo.tabstop = 2
    vim.bo.softtabstop = 2
    vim.bo.expandtab = true
  end,
})

