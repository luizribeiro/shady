# ⛱ shady

[![CI](https://github.com/luizribeiro/shady/actions/workflows/ci.yml/badge.svg)](https://github.com/luizribeiro/shady/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/luizribeiro/shady/branch/main/graph/badge.svg?token=25KwvC0cSi)](https://codecov.io/gh/luizribeiro/shady)

Shady is a typed scripting language designed for system automation and command orchestration. It combines the expressiveness of traditional shell scripts with a robust type system and first-class process management.

## Why Shady?

Traditional shell scripts are powerful but lack type safety and structured process management. Shady addresses these limitations by providing:

- **Type Safety**: Catch errors before runtime with a static type system (int, str, bool, list, proc, functions)
- **First-Class Processes**: Process handles (`Proc` type) with built-in support for stdin/stdout/stderr redirection
- **Functional Programming**: Lambda expressions with closures and higher-order functions (map, filter, reduce)
- **Automatic CLI Generation**: Public functions become CLI subcommands automatically with proper argument parsing
- **IDE Support**: Built-in LSP server with auto-completion, type checking, and signature help
- **Clean Syntax**: Readable, structured syntax with string interpolation and functional composition
- **Seamless External Commands**: Call any external command as if it were a native function

## Quick Start

### Installation

```bash
# Enter development environment (requires Nix with devenv)
devenv shell

# Build shady
cargo build

# Run a script
cargo run -- examples/example.shady
```

### Your First Script

Create a file `hello.shady`:

```shady
# Simple function that prints a greeting using string interpolation
public greet $name: str = print "Hello, {$name}!";

# Combines type safety with external command execution
public greet-uppercase $name: str = exec (echo "Hello, {$name}!" > tr "a-z" "A-Z");
```

Run it as a CLI:

```bash
./shady hello.shady greet "World"              # Hello, World!
./shady hello.shady greet-uppercase "World"    # HELLO, WORLD!
```

## Features

### Type System

Shady supports the following types:

- `int` - 64-bit integers
- `str` - UTF-8 strings
- `bool` - Boolean values
- `[T]` - Lists of any type
- `proc` - Process handles with I/O streams
- `fn(T1, T2, ...) -> R` - Function types (lambdas)

```shady
# Type-safe calculator with proper precedence
# Returns ($x + $y) * 8
public calculate $x: int $y: int -> int = $x + $y * 2 ^ 3;

# Type errors are caught at parse/eval time
# Returns 0 for division by zero
public safe-divide $a: int $b: int -> int = if ($b == 0) 0 else $a / $b;

# Lambda expressions enable functional composition
public double-twice $x: int -> int = first (map (lambda $y -> $y * 2) (map (lambda $z -> $z * 2) [$x]));
```

### String Interpolation

Embed expressions directly in strings using `{expr}` syntax. Any expression can be interpolated, and the result is automatically converted to a string:

```shady
# Simple variable interpolation
public greet $name: str = print "Hello, {$name}!";

# Arithmetic expressions
public show-result $x: int $y: int = print "{$x} + {$y} = {$x + $y}";

# Nested expressions and function calls
public deployment-status $version: str $server: str = print "Deploying v{$version} to {$server} at {stdout (date)}";

# Boolean expressions
public check-value $x: int = print ("Value " + to_string $x + " is " + if ($x > 10) "large" else "small");

# Mix interpolation with literals
public format-list $count: int = print ("Found " + to_string $count + " item" + if ($count == 1) "" else "s");
```

String interpolation makes string building more readable compared to concatenation:

```shady
# Before (string concatenation)
print ("User " + $name + " logged in at " + (stdout (date)));

# After (string interpolation)
print "User {$name} logged in at {stdout (date)}";
```

### Process Management

External commands are first-class citizens. Shady automatically spawns processes and provides access to their streams:

```shady
# Capture and process command output
public get-git-branch = stdout (git branch --show-current);

# Chain processes with type-safe redirection
public count-errors $logfile: str = stdout (cat $logfile > grep "ERROR" > wc -l);

# Compose complex pipelines naturally
public top-committers = lines (git log --oneline > head -10);

# Mix typed data with process output
public deploy-info $version: str = print ("Deploying version " + $version + " to " + get-git-branch);
```

### Sequential Execution

Use `seq` to orchestrate complex workflows:

```shady
# Automated deployment pipeline
public deploy $server: str $version: str = seq [
  echo ("Starting deployment of v" + $version + " to " + $server);
  git fetch --tags;
  git checkout $version;
  cargo test;
  cargo build --release;
  echo "Deployment complete!";
];

# Database backup with verification
public backup-database $name: str = seq [
  echo ("Backing up database: " + $name);
  pg_dump $name;
  echo "Backup complete";
];
```

### Conditional Expressions

```shady
# If expressions are values, enabling functional composition
public max $a: int $b: int -> int =
  if ($a > $b) $a else $b;

# Smart deployment based on environment
public deploy-to-env $env: str =
  if ($env == "prod")
    seq [
      echo "Deploying to production - running full test suite...";
      cargo test;
      cargo build --release;
      ssh prod-server "systemctl stop app";
      scp target/release/app prod-server:/opt/app/;
      ssh prod-server "systemctl start app";
    ]
  else
    seq [
      echo "Deploying to staging - quick deploy";
      cargo build;
      scp target/debug/app staging-server:/opt/app/;
      ssh staging-server "systemctl restart app";
    ];

# Conditional string formatting
public format-count $count: int =
  print (
    (to_string $count) +
    (if ($count == 1) " item" else " items")
  );
```

### Automatic CLI Generation

Public functions automatically become CLI subcommands with proper argument parsing:

```shady
# Default values make parameters optional
public serve $port: int (8080, option) = echo ("Starting server on port " + to_string $port);

# Multiple optional parameters with sensible defaults
public docker-build $name: str $tag: str ("latest", option) = echo ("Building " + $name + " with tag " + $tag);
```

Use from the command line:

```bash
# Use defaults
./script.shady serve
# Starting server on localhost:8080

# Override specific options
./script.shady serve --port 3000 --host "0.0.0.0"
# Starting server on 0.0.0.0:3000

# Required and optional parameters
./script.shady docker-build myapp --tag "v1.2.3"
```

### Recursion and Private Functions

```shady
# Private helper for recursive computation (not exposed to CLI)
factorial_impl $n: int $acc: int -> int =
  if ($n <= 1)
    $acc
  else
    factorial_impl ($n - 1) ($acc * $n);

# Public interface with nice output
public factorial $n: int =
  print "{$n}! = {factorial_impl $n 1}";

# Recursive file processing
count_lines_impl $files: [str] $total: int -> int =
  if ((first $files) == "")
    $total
  else
    count_lines_impl
      (rest $files)
      ($total + (stdout (wc -l (first $files) > awk "{print $1}")));

public count-project-lines =
  print (
    "Total lines: " +
    (to_string (count_lines_impl (lines (find . -name "*.rs")) 0))
  );
```

### Lambda Expressions and Higher-Order Functions

Shady supports first-class lambda expressions with closures, enabling functional programming patterns:

```shady
# Lambda syntax: using the lambda keyword
public double-all $nums: [int] =
  map (lambda $x -> $x * 2) $nums;

# Lambdas can capture variables from their environment (closures)
public add-to-all $n: int $nums: [int] =
  map (lambda $x -> $x + $n) $nums;

# Filter with predicates
public get-positives $nums: [int] =
  filter (lambda $x -> $x > 0) $nums;

# Reduce for aggregation
public sum-list $nums: [int] =
  reduce (lambda $acc $x -> $acc + $x) 0 $nums;

# Combine higher-order functions
public process-numbers $nums: [int] =
  reduce
    (lambda $acc $x -> $acc + $x)
    0
    (filter (lambda $x -> $x > 0) (map (lambda $x -> $x * 2) $nums));

# Type annotations are optional (inferred from context)
public explicit-types $nums: [int] =
  map (lambda $x: int -> int = $x + 1) $nums;

# Multi-parameter lambdas (simplified example)
public zip-sum $a: [int] $b: [int] =
  reduce (lambda $acc $pair -> $acc + (first $pair) + (first (rest $pair))) 0 [];
```

### List Operations

```shady
# List literals use semicolon separators (prints 15)
public sum-numbers = print (to_string (add_all [1; 2; 3; 4; 5]));

# Transform lists with map
public square-all $nums: [int] = map (lambda $x -> $x * $x) $nums;

# Filter lists with predicates
public get-evens $nums: [int] = filter (lambda $x -> ($x % 2) == 0) $nums;

# Lists from CLI are comma-separated
public batch-process $files: [str] = seq [
  echo ("Processing " + (to_string (add_all [1])) + " files...");
  first $files;
];

# Combine lists with process output
public analyze-git-files $extensions: [str] =
  lines (
    find . -name ("*." + (first $extensions))
      -not -path "./.git/*"
  );

# Dynamic list building from command output
public modified-files =
  lines (git diff --name-only);

# Functional data processing
public count-large-files $threshold: int =
  reduce
    (lambda $count $_ -> $count + 1)
    0
    (filter
      (lambda $size -> $size > $threshold)
      (map (lambda $line -> first (lines $line)) (lines (du -b *)))
    );
```

Call from CLI:

```bash
# Lists are comma-separated
./script.shady batch-process "main.rs,lib.rs,mod.rs"

# Multiple extensions
./script.shady analyze-git-files "rs,toml"
```

### Environment Variables

```shady
# Access environment variables with defaults
public get-editor = env "EDITOR" "vim";

# Build dynamic paths from environment
public backup-to-home $filename: str = exec (cp $filename (env "HOME" "/tmp" + "/backup/" + $filename));

# Environment-aware configuration
public configure = if (env "ENV" "dev" == "prod") echo "Using production config" else echo "Using development config";
```

## Real-World Examples

### Infrastructure Management

```shady
# Multi-server deployment with health checks
public deploy-cluster $version: str = seq [
  echo ("Deploying " + $version + " to cluster");
  docker pull myapp;
  docker compose up -d;
  echo "Cluster deployment complete!";
];

# Automated backup with rotation
public backup-all = seq [
  echo ("Starting backup at " + stdout (date));
  pg_dump production;
  tar czf backup.tgz /var/www;
  echo "Backup complete";
];
```

### Development Workflow Automation

```shady
# Pre-commit validation
public pre-commit = seq [
  echo "Running pre-commit checks...";
  cargo fmt --check;
  cargo clippy -- -D warnings;
  cargo test;
  echo "All checks passed!";
];

# Smart test runner with coverage
public test-with-coverage $pattern: str ("", option) = seq [
  if ($pattern == "")
    cargo test
  else
    cargo test $pattern;
  cargo tarpaulin --out Html;
  echo ("Coverage report: " + (stdout (pwd)) + "/tarpaulin-report.html");
];

# Release automation
public release $version: str = seq [
  echo ("Preparing release " + $version);
  git tag -a $version -m ("Release " + $version);
  cargo build --release;
  cargo publish;
  git push origin $version;
  gh release create $version target/release/app;
];
```

### Log Analysis and Monitoring

```shady
# Real-time error monitoring
public watch-errors $logfile: str =
  exec (tail -f $logfile > grep --color=always "ERROR");

# Aggregate error counts by type
public error-summary $logfile: str =
  stdout (
    cat $logfile >
    grep "ERROR" >
    awk "{print $5}" >
    sort >
    uniq -c >
    sort -rn
  );

# Alert on threshold
public check-error-rate $logfile: str $threshold: int = seq [
  echo "Checking error rate...";
  if ((stdout (cat $logfile > grep -c "ERROR")) > $threshold)
    seq [
      echo "ALERT: Error threshold exceeded!";
      curl -X POST https://alerts.example.com/webhook;
    ]
  else
    echo "Error rate within normal limits";
];
```

### Functional Data Processing

```shady
# Process lists of files with functional composition
public analyze-files $files: [str] = map (lambda $file -> $file + ": processed") $files;

# Transform and filter with map
public process-numbers $nums: [int] = filter (lambda $x -> $x > 10) (map (lambda $y -> $y * 2) $nums);

# Parallel-style processing with map
public format-list $items: [str] = map (lambda $item -> "Item: " + $item) $items;

# Compute statistics with reduce
public sum-and-double $nums: [int] -> int = reduce (lambda $acc $x -> $acc + $x * 2) 0 $nums;
```

## IDE Support

Shady includes a Language Server Protocol (LSP) implementation for enhanced IDE integration:

- **Auto-completion**: Context-aware suggestions for functions, variables, and types
- **Signature Help**: Real-time parameter hints while typing function calls
- **Go to Definition**: Jump to function definitions
- **Type Checking**: Real-time type error detection
- **Hover Information**: Type information on hover

The LSP automatically discovers all builtins (including user-defined ones via macros), so IDE features stay in sync with language extensions.

## Planned Features

- Variadic functions
- Optional function parameters (beyond default values)
- Stream support for builtins and local functions
- Enhanced error messages with miette
- Advanced I/O redirection from seq blocks
- More comprehensive standard library

## Development

```bash
# Run tests
devenv shell cargo test

# Run with AST output for debugging
devenv shell cargo run -- --ast script.shady

# Read script from stdin
echo 'public main = print "Hello";' | devenv shell cargo run -- - main
```

## Contributing

Contributions are welcome! This is a prototype project exploring the intersection of type systems and shell scripting. Feel free to open issues or pull requests.

## License

See LICENSE file for details.
