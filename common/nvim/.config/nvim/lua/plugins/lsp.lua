return {
  "neovim/nvim-lspconfig",
  config = function()
    local lspconfig = vim.lsp.config()
    local on_attach = function(client, bufnr)
      local buf_map = function(mode, lhs, rhs, desc)
        vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
      end

      buf_map("n", "<leader>gd", vim.lsp.buf.definition, "Go to definition")
      buf_map("n", "K", vim.lsp.buf.hover, "Show hover")
    end
    local capabilities = require('cmp_nvim_lsp').default_capabilities()
    
    -- Python LSP
    lspconfig.pylsp.setup({
      on_attach = on_attach,
      capabilities=capabilities
    })

    -- TypeScript / JavaScript LSP (typescript-language-server)
    lspconfig.ts_ls.setup({
      on_attach = on_attach,
      capabilities=capabilities
    })
    -- Go LSP (gopls)
    lspconfig.gopls.setup({
      on_attach = on_attach,
      capabilities = capabilities,
    })

    -- Go linting (golangci-lint) 
    lspconfig.golangci_lint_ls.setup({
      on_attach = on_attach,
      capabilities = capabilities,
    })
    -- HTML LSP
    lspconfig.html.setup({
      on_attach = on_attach,
      capabilities=capabilities
    })
  end,
}

