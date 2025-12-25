return {
  "neovim/nvim-lspconfig",
  config = function()
    local on_attach = function(client, bufnr)
      local buf_map = function(mode, lhs, rhs, desc)
        vim.keymap.set(mode, lhs, rhs, { buffer = bufnr, desc = desc })
      end

      buf_map("n", "<leader>gd", vim.lsp.buf.definition, "Go to definition")
      buf_map("n", "K", vim.lsp.buf.hover, "Show hover")
    end

    local capabilities = require("cmp_nvim_lsp").default_capabilities()

    -- Python
    vim.lsp.config("pylsp", { 
	on_attach = on_attach, 
	capabilities = capabilities 
    })

    -- TS / JS
    vim.lsp.config("ts_ls", { 
	on_attach = on_attach, 
	capabilities = default_capabilities	
    })

    -- Go
    vim.lsp.config("gopls", { 
	on_attach = on_attach, 
	capabilities = default_capabilities
    })

    -- Go lint
    vim.lsp.config("golangci_lint_ls", { 
	on_attach = on_attach, 
	capabilities = capabilities 
    })

    -- HTML
    vim.lsp.config("html", { 
	on_attach = on_attach, 
	capabilities = capabilities 
    })

    -- Enable servers
    vim.lsp.enable({ 
	"pylsp", 
	"ts_ls", 
	"gopls", 
	"golangci_lint_ls", 
	"html" 
    })
  end,
}
