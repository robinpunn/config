# neovim-config

## Features

- **Colorscheme:** 
    - tokyonight
- **Line numbers**
- **Treesitter:** 
    - syntax highlighting
    - code folding
    - indentation
- **LSP (Language Server Protocol):** 
    - go to definition  
    - python (pylsp)
    - javaScript/typeScript (ts_ls)
- **Nvm-Cmp (auto-complete):**
    - cmp-buffer (buffer words)
    - cmp-path (file system paths)
    - cmp-nvim-lsp (neovim built in language server)
    - LuaSnip (snippet engine)
    - friendly-snippets (preconfigured snippets for different languages)
- **Fugitive**
    - lazy load on `<leader>gs` or `<cmd>G`
- **Telescope**
    - fzf
- **Oil**
    - web devicons  
- **Keymaps:**  
    - diagnostics navigation
    - quick access to location list

## Structure
`~/.config/nvim/`
```
├── init.lua
└── lua/
  ├── config/
  │ └── lazy.lua
  ├── core/
  │ ├── options.lua 
  │ ├── keymaps.lua 
  │ └── autocmds.lua 
  └── plugins/
    ├── treesitter.lua
    ├── lsp.lua 
    ├── cmp.lua
    ├── oil.lua
    ├── telescope.lua
    ├── fugitive.lua
    └── tokyonight.lua
```
