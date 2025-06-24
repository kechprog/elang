# Elang Language Server Protocol (LSP)

A Language Server Protocol implementation for the Elang programming language, providing rich IDE features for any LSP-compatible editor.

## Features

### Core LSP Features
- **Document Synchronization**: Real-time document tracking (textDocument/didOpen, didChange, didClose)
- **Syntax Error Diagnostics**: Real-time syntax error detection and reporting
- **Hover Information**: Type information and documentation on hover
- **Auto-completion**: Context-aware code completion with snippets
- **Go to Definition**: Navigate to symbol definitions
- **Find References**: Find all references to a symbol
- **Document Symbols**: Outline view with functions, structs, and variables
- **Workspace Symbols**: Search symbols across the workspace
- **Code Formatting**: Basic code formatting support

### Elang-Specific Features
- **Built-in Function Completion**: Auto-completion for Elang built-in functions
- **Type Inference**: Basic type inference for hover information
- **Keyword Highlighting**: Support for all Elang keywords
- **Code Snippets**: Pre-defined code snippets for common patterns

## Installation

### Building from Source

```bash
# Clone the repository
git clone <repository-url>
cd elang/elang_lsp

# Build the LSP server
cargo build --release

# The binary will be available at target/release/elang_lsp
```

### Running the LSP Server

The LSP server communicates via stdin/stdout:

```bash
./target/release/elang_lsp
```

## Editor Integration

### VS Code

Create a VS Code extension or use a generic LSP client extension:

1. Install the "Generic LSP Client" extension
2. Configure it to use the `elang_lsp` binary
3. Set the language ID to "elang"

Example configuration in VS Code settings:

```json
{
  "genericLspClient.languageConfigs": {
    "elang": {
      "command": "/path/to/elang_lsp",
      "args": [],
      "filetypes": ["elang"]
    }
  }
}
```

### Neovim

Using nvim-lspconfig:

```lua
local lspconfig = require('lspconfig')

local configs = require('lspconfig.configs')
if not configs.elang_lsp then
  configs.elang_lsp = {
    default_config = {
      cmd = { '/path/to/elang_lsp' },
      filetypes = { 'elang' },
      root_dir = lspconfig.util.root_pattern('.git', 'Cargo.toml'),
      settings = {},
    },
  }
end

lspconfig.elang_lsp.setup{}
```

### Emacs

Using lsp-mode:

```elisp
(add-to-list 'lsp-language-id-configuration '(elang-mode . "elang"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "/path/to/elang_lsp")
                  :major-modes '(elang-mode)
                  :server-id 'elang-lsp))
```

## Supported LSP Methods

### Lifecycle
- `initialize`
- `initialized`
- `shutdown`
- `exit`

### Document Synchronization
- `textDocument/didOpen`
- `textDocument/didChange`
- `textDocument/didClose`

### Language Features
- `textDocument/hover`
- `textDocument/completion`
- `textDocument/definition`
- `textDocument/references`
- `textDocument/documentSymbol`
- `workspace/symbol`
- `textDocument/formatting`
- `textDocument/rangeFormatting`

### Workspace Features
- `workspace/didChangeConfiguration`
- `workspace/didChangeWatchedFiles`

## Configuration

The LSP server can be configured through initialization options:

```json
{
  "initializationOptions": {
    "diagnostics": {
      "enable": true,
      "level": "error"
    },
    "completion": {
      "enable": true,
      "snippets": true
    },
    "hover": {
      "enable": true,
      "showTypes": true
    }
  }
}
```

## Development

### Project Structure

```
elang_lsp/
├── src/
│   ├── main.rs          # Entry point
│   ├── lib.rs           # Main LSP backend
│   ├── analysis.rs      # Semantic analysis
│   ├── completion.rs    # Auto-completion provider
│   └── hover.rs         # Hover information provider
├── tests/
│   └── integration_tests.rs  # Integration tests
├── Cargo.toml
└── README.md
```

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_lsp_initialization

# Run with logging
RUST_LOG=debug cargo test
```

### Debugging

Enable debug logging:

```bash
RUST_LOG=elang_lsp=debug ./target/release/elang_lsp
```

The LSP server will log to stderr, which most editors capture separately from the LSP communication.

## Dependencies

- **elang_ast**: Core AST definitions for Elang
- **elang_parser**: Parser for Elang source code
- **tower-lsp**: LSP framework for Rust
- **tokio**: Async runtime
- **serde_json**: JSON serialization
- **dashmap**: Concurrent HashMap for document storage
- **tracing**: Structured logging

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass
6. Submit a pull request

### Adding New LSP Features

To add a new LSP feature:

1. Add the method to the `LanguageServer` trait implementation in `lib.rs`
2. Implement the logic in the appropriate module (`analysis.rs`, `completion.rs`, etc.)
3. Add tests in `tests/integration_tests.rs`
4. Update this README with the new feature

## Troubleshooting

### Common Issues

1. **LSP server not starting**: Check that the binary path is correct and executable
2. **No completions**: Ensure the document is properly opened and parsed
3. **Syntax errors not showing**: Check that diagnostics are enabled in your editor
4. **Hover not working**: Verify the cursor is on a valid symbol

### Logging

Enable detailed logging to diagnose issues:

```bash
RUST_LOG=tower_lsp=debug,elang_lsp=debug ./target/release/elang_lsp
```

### Performance

For large files or workspaces:
- The LSP server uses incremental parsing where possible
- Document analysis is performed asynchronously
- Consider adjusting editor LSP timeout settings for large files

## License

This project is licensed under the same license as the main Elang project.

## Roadmap

### Planned Features
- **Incremental Parsing**: More efficient parsing for large files
- **Advanced Type Checking**: Full type system integration
- **Code Actions**: Quick fixes and refactoring suggestions
- **Semantic Highlighting**: Enhanced syntax highlighting
- **Inlay Hints**: Type hints and parameter names
- **Call Hierarchy**: Function call relationships
- **Folding Ranges**: Code folding support
- **Rename Symbol**: Safe symbol renaming
- **Workspace-wide Analysis**: Cross-file symbol resolution

### Performance Improvements
- Caching of parsed ASTs
- Lazy loading of workspace files
- Background analysis for better responsiveness