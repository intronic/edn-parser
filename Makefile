.PHONY: build clean test generate-js generate-ts install

# Build the Haskell project
build:
	cabal build

# Clean build artifacts
clean:
	cabal clean

# Run tests
test:
	cabal test

# Install dependencies and build
install:
	cabal update
	cabal install --dependencies-only
	cabal build

# Generate JavaScript parser library
generate-js:
	cabal run edn-codegen -- --language js --output edn-parser.js

# Generate TypeScript parser library
generate-ts:
	cabal run edn-codegen -- --language ts --output edn-parser.ts

# Generate both JS and TS libraries
generate-all: generate-js generate-ts

# Example usage
example:
	@echo "Generating JavaScript library..."
	make generate-js
	@echo "Generating TypeScript library..."
	make generate-ts
	@echo "Done! Generated edn-parser.js and edn-parser.ts"