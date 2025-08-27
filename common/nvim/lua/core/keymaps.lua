--dx messages
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
--jump to dx
vim.keymap.set('n', "]g", function() vim.diagnostic.jump({count = 1}) end)
vim.keymap.set('n', "[g", function() vim.diagnostic.jump({count = -1}) end)
--dx message on jump
vim.diagnostic.config({
    jump = {float = true}
})

-- netrw
vim.keymap.set("n", "<C-b>", ":Lex<CR>", { noremap = true, silent = true, desc = "Toggle netrw Lex" })
vim.keymap.set("n", "<leader>b", ":Ex<CR>", { noremap = true, silent = true, desc = "Open netrw Ex" })
vim.keymap.set("n", "<leader>B", ":Vex<CR>", { noremap = true, silent = true, desc = "Open netrw Vex" })

-- tabs
vim.keymap.set("n", "<leader>gT", ":tabmove -1<CR>", { noremap = true , desc = "Move tab left" })
vim.keymap.set("n", "<leader>gt", ":tabmove +1<CR>", { noremap = true, desc= "Move tab right" })

-- spell check
vim.keymap.set("n", "<leader>S", ":set spell!<CR>", {noremap = true, desc = "Toggle spell check"})
