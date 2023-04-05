stack_build := "stack build --fast"

# No default tasks
default:
  just --list

# Build and run tests
test:
  {{ stack_build }} --test

# Build only
build:
  {{ stack_build }} --test --no-run-tests

# Clean stack work
clean:
  stack clean --full

# Enter repl
ghci:
  stack ghci --test

# Open browser with generated docs
docs:
  stack haddock --open

# Install tool deps
deps:
  #!/bin/bash
  set -eux
  # Install simple deps
  stack build --copy-compiler-tool hlint fourmolu
  # Install haskell-ci
  if ! command -v haskell-ci &> /dev/null
  then
    rm -rf /tmp/haskell-ci && cd /tmp
    git clone git@github.com:haskell-CI/haskell-ci.git
    cd haskell-ci
    cabal install haskell-ci
    cd .. && rm -rf haskell-ci
  fi

# Generate Haskell CI config
gen-ci:
  stack setup
  haskell-ci github midiot.cabal

# Format with fourmolu
format:
  stack exec -- fourmolu --mode inplace src test exe

# Lint with hlint
lint:
  stack exec -- hlint src test exe

# Run the executable
exe:
  {{ stack_build }} --test --no-run-tests --exec midiot-exe
