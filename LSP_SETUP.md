# Shady LSP Server Setup

This guide will help you set up the Shady Language Server Protocol (LSP) server with your editor.

## Building the LSP Server

```bash
devenv shell cargo build --bin shady-lsp --release
```

The binary will be located at: `target/release/shady-lsp`

## Features

The Shady LSP server currently supports:

1. **Document Synchronization** - Keeps track of open Shady files
2. **Diagnostics** - Shows syntax errors in real-time as you type
3. **Hover Information** - Shows function signatures when hovering over code (basic implementation)
4. **Go to Definition** - Jump to function definitions by clicking on function names

## Editor Configuration

### VS Code

Create or edit `.vscode/settings.json` in your project:

```json
{
  "shady.lsp.path": "/path/to/shady/target/release/shady-lsp",
  "files.associations": {
    "*.shady": "shady"
  }
}
```

Then install a generic LSP extension or create a simple extension that launches the server for `.shady` files.

### Neovim (with nvim-lspconfig)

Add to your Neovim configuration:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Define the shady LSP
if not configs.shady then
  configs.shady = {
    default_config = {
      cmd = {'/path/to/shady/target/release/shady-lsp'},
      filetypes = {'shady'},
      root_dir = lspconfig.util.root_pattern('.git', 'CLAUDE.md'),
      settings = {},
    },
  }
end

-- Setup the shady LSP
lspconfig.shady.setup{}

-- Set filetype for .shady files
vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = "*.shady",
  callback = function()
    vim.bo.filetype = "shady"
  end,
})
```

### Emacs (with lsp-mode)

Add to your Emacs configuration:

```elisp
(require 'lsp-mode)

(add-to-list 'lsp-language-id-configuration '(shady-mode . "shady"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/path/to/shady/target/release/shady-lsp")
                  :major-modes '(shady-mode)
                  :server-id 'shady-lsp))

(add-to-list 'auto-mode-alist '("\\.shady\\'" . shady-mode))
```

## Testing the LSP Server

Create a test file `test.shady`:

```shady
# Valid syntax
public main = echo "Hello, World!";

add $a: int $b: int = $a + $b;
```

This should show no errors.

Now try adding invalid syntax:

```shady
# Invalid - missing semicolon
public main = echo "Hello, World!"
```

You should see a parse error diagnostic.

## Current Limitations

- Hover information is basic (shows first function signature)
- No autocompletion
- No code actions
- No formatting

## Future Enhancements

Possible improvements:

1. **Find References** - Find all uses of a function
2. **Autocompletion** - Suggest function names, variables, and builtin functions
3. **Signature Help** - Show parameter information while typing function calls
4. **Code Actions** - Quick fixes for common errors
5. **Formatting** - Auto-format Shady code
6. **Rename** - Rename functions and variables across the file
7. **Document Symbols** - Show outline of functions in the file

## Development

The LSP server code is in:
- `src/lsp.rs` - Main LSP implementation
- `src/bin/lsp.rs` - Binary entry point

To test changes:

```bash
# Build
devenv shell cargo build --bin shady-lsp

# Run manually (for debugging)
devenv shell cargo run --bin shady-lsp
# Then send LSP messages via stdin
```

## Troubleshooting

### Server not starting

Check that the path to `shady-lsp` is correct and the binary is executable:

```bash
chmod +x target/release/shady-lsp
./target/release/shady-lsp
```

The server should start and wait for LSP messages on stdin.

### No diagnostics showing

Make sure your editor is configured to:
1. Recognize `.shady` files
2. Start the LSP server for `.shady` files
3. Display diagnostics from the LSP server

Check your editor's LSP logs for error messages.
