# Elang Programming Language

Elang is a modern, statically-typed programming language with a focus on simplicity, safety, and developer productivity. This repository contains the complete Elang toolchain including the language implementation, parser, interpreter, and Language Server Protocol (LSP) support.

## Project Overview

The Elang language provides:

- **Static typing** with type inference
- **Pattern matching** with destructuring
- **First-class functions** and closures
- **Struct-based data modeling**
- **Module system** with explicit imports/exports
- **Generic programming** support
- **Comprehensive tooling** including LSP support for editors

## Architecture

The project is organized as a Rust workspace with four main crates:

```
elang/
├── elang_ast/          # Abstract Syntax Tree definitions
├── elang_parser/       # Parser implementation
├── elang_interpreter/  # Language interpreter
├── elang_lsp/          # Language Server Protocol implementation
└── Cargo.toml          # Workspace configuration
```

### Dependency Graph

```
elang_lsp → elang_parser → elang_ast
    ↓           ↓
elang_interpreter → elang_ast
```

## Components

### elang_ast

The core AST (Abstract Syntax Tree) crate that defines all language constructs:

- **Expressions**: Literals, variables, function calls, conditionals, etc.
- **Patterns**: For destructuring and pattern matching
- **Types**: Type system representation including generics
- **Declarations**: Functions, structs, variables, and modules
- **Modules**: Top-level program structure

**Key Features:**
- 25 comprehensive tests covering all AST node types
- Support for generic types and pattern matching
- Clean separation between expressions, types, and declarations

### elang_parser

A robust parser that converts Elang source code into AST:

- **Recursive descent parser** with proper error handling
- **Complete language support** including all syntax features
- **Comprehensive testing** with 30 test cases
- **Error recovery** for better developer experience

**Key Features:**
- Parses all Elang language constructs
- Handles complex expressions and nested structures
- Supports type annotations and generic syntax
- Provides meaningful error messages

### elang_interpreter

A tree-walking interpreter that executes Elang programs:

- **Direct AST interpretation** for rapid development
- **Type checking** during execution
- **Environment management** for variable scoping
- **Built-in functions** and standard library
- **Module system** with import/export support

**Key Features:**
- 57 comprehensive tests covering all language features
- Support for closures and higher-order functions
- Pattern matching with destructuring
- Generic struct instantiation
- Comprehensive error handling

### elang_lsp

A Language Server Protocol implementation providing IDE support:

- **Real-time syntax analysis** and error reporting
- **Code completion** with context-aware suggestions
- **Hover information** for symbols and keywords
- **Go-to-definition** and find references
- **Document symbols** and workspace symbols
- **Code formatting** and refactoring support

**Key Features:**
- 5 integration tests covering core LSP functionality
- Full LSP protocol compliance
- Asynchronous document processing
- Extensible architecture for additional features

## Building

### Prerequisites

- Rust 1.70+ with Cargo
- Git

### Build All Components

From the project root:

```bash
# Build all crates in development mode
cargo build

# Build all crates in release mode (optimized)
cargo build --release

# Run all tests
cargo test
```

### Build Individual Crates

```bash
# Build specific crates
cd elang_ast && cargo build
cd elang_parser && cargo build
cd elang_interpreter && cargo build
cd elang_lsp && cargo build --release
```

## Usage

### Running the Interpreter

```bash
# Build the interpreter
cd elang_interpreter
cargo build --release

# Run an Elang program
./target/release/elang program.elang

# Interactive REPL mode
./target/release/elang
```

### Using the LSP Server

The LSP server can be integrated with any editor that supports LSP:

```bash
# Build the LSP server
cd elang_lsp
cargo build --release

# The binary will be available at:
./target/release/elang_lsp.exe  # Windows
./target/release/elang_lsp      # Unix/Linux/macOS
```

#### VS Code Integration

Create a VS Code extension or configure a generic LSP client to use:
- **Command**: `path/to/elang_lsp`
- **File patterns**: `*.elang`

#### Vim/Neovim Integration

Use with `nvim-lspconfig` or similar:

```lua
require'lspconfig'.elang.setup{
  cmd = {"path/to/elang_lsp"},
  filetypes = {"elang"},
}
```

## Language Examples

### Basic Syntax

```elang
// Variable declarations
let x = 42;
let name = "Elang";
let is_ready = true;

// Function definitions
fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

// Struct definitions
struct Point {
    x: i32,
    y: i32,
}

// Struct instantiation
let origin = Point { x: 0, y: 0 };
```

### Pattern Matching

```elang
// Option type handling
let maybe_value = Some(42);

if let Some(value) = maybe_value {
    println("Value: {}", value);
} else {
    println("No value");
}

// Struct destructuring
let Point { x, y } = origin;
```

### Generics

```elang
// Generic struct
struct Container<T> {
    value: T,
}

// Generic instantiation
let int_container = Container { value: 42 };
let string_container = Container { value: "hello" };
```

### Modules

```elang
// math.elang
provide add, multiply;

fn add(a: i32, b: i32) -> i32 {
    return a + b;
}

fn multiply(a: i32, b: i32) -> i32 {
    return a * b;
}

// main.elang
require add from "math.elang";

fn main() {
    let result = add(2, 3);
    println("Result: {}", result);
}
```

## Development

### Project Structure

- **Source code**: Each crate has its source in `src/`
- **Tests**: Unit tests in `src/` and integration tests in `tests/`
- **Examples**: Sample Elang programs in `examples/`
- **Documentation**: API docs generated with `cargo doc`

### Running Tests

```bash
# Run all tests
cargo test

# Run tests for specific crate
cd elang_ast && cargo test
cd elang_parser && cargo test
cd elang_interpreter && cargo test
cd elang_lsp && cargo test

# Run with output
cargo test -- --nocapture
```

### Adding Language Features

1. **Define AST nodes** in `elang_ast/src/lib.rs`
2. **Add parsing logic** in `elang_parser/src/lib.rs`
3. **Implement interpretation** in `elang_interpreter/src/interpreter.rs`
4. **Add LSP support** in `elang_lsp/src/` modules
5. **Write comprehensive tests** for all components

### Code Style

- Follow Rust standard formatting (`cargo fmt`)
- Use `cargo clippy` for linting
- Maintain comprehensive test coverage
- Document public APIs with doc comments

## Testing

The project includes comprehensive test suites:

- **elang_ast**: 25 tests covering all AST node types
- **elang_parser**: 30 tests covering parsing functionality
- **elang_interpreter**: 57 tests covering language semantics
- **elang_lsp**: 5 integration tests covering LSP functionality

**Total**: 117 tests ensuring robust language implementation

### Test Categories

- **Unit tests**: Test individual functions and methods
- **Integration tests**: Test component interactions
- **Language specification tests**: Verify language behavior
- **Error handling tests**: Ensure proper error reporting

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

### Guidelines

- Maintain backward compatibility
- Add comprehensive tests
- Update documentation
- Follow existing code style
- Consider performance implications

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Roadmap

### Short Term
- [ ] Improved error messages with source locations
- [ ] More built-in functions and standard library
- [ ] Better LSP diagnostics and quick fixes

### Medium Term
- [ ] Compile-time optimizations
- [ ] Package manager and module system
- [ ] Debugging support in LSP

### Long Term
- [ ] Native code compilation
- [ ] Advanced type system features
- [ ] IDE plugins for major editors

## Acknowledgments

Built with Rust and the following excellent crates:
- `tower-lsp` for LSP implementation
- `serde` for serialization
- `tokio` for async runtime
- `dashmap` for concurrent data structures