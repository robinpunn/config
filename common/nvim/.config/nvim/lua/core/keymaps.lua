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
vim.keymap.set("n", "<C-b>", "<cmd>Lex<CR>", { silent = true, desc = "Toggle netrw Lex" })
vim.keymap.set("n", "<leader>b", "<cmd>Ex<CR>", { silent = true, desc = "Open netrw Ex" })
vim.keymap.set("n", "<leader>B", "<cmd>Vex<CR>", { silent = true, desc = "Open netrw Vex" })

-- buffer fast travel
vim.keymap.set("n", "[b", "<cmd>bprevious<CR>", { silent = true, desc = "Go to previous buffer" })
vim.keymap.set("n", "]b", "<cmd>bnext<CR>", { silent = true, desc = "Go to next buffer" })

-- tabs
vim.keymap.set("n", "<leader>gT", "<cmd>tabmove -1<CR>", { desc = "Move tab left" })
vim.keymap.set("n", "<leader>gt", "<cmd>tabmove +1<CR>", { desc = "Move tab right" })

-- quickfix
vim.keymap.set("n", "<leader>qo", "<cmd>copen<CR>",  { desc = "Quickfix open" })
vim.keymap.set("n", "<leader>qc", "<cmd>cclose<CR>", { desc = "Quickfix close" })
vim.keymap.set("n", "<leader>qn", "<cmd>cnext<CR>",  { desc = "Quickfix next" })
vim.keymap.set("n", "<leader>qp", "<cmd>cprev<CR>",  { desc = "Quickfix prev" })
vim.keymap.set("n", "<leader>qz", "<cmd>cgetexpr []<CR>",  { desc = "Quickfix clear" })

-- remap C-z (so i dont keep accidently closing nvim)
vim.keymap.set("n", "<C-z>", "<cmd>u<CR>", { desc = "Undo" })

-- spell check
vim.keymap.set("n", "<leader>S", ":set spell!<CR>", { desc = "Toggle spell check" })
