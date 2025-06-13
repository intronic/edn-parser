# EDN Parser Generator

A Haskell library that generates JavaScript and TypeScript libraries for parsing EDN (Extensible Data Notation) files from Clojure and Datomic.

## Features

- Complete EDN parser implementation in Haskell
- Generates optimized JavaScript and TypeScript parsing libraries
- Supports all EDN data types:
  - nil, booleans, strings, characters, numbers
  - Keywords and symbols
  - Lists, vectors, sets, and maps
  - Tagged values (including #inst and #uuid)
  - Comments

## Quick Start

### Prerequisites

- GHC (Glasgow Haskell Compiler) >= 8.10
- Cabal >= 3.0

### Build

```bash
# Install dependencies and build
make install

# Or manually:
cabal update
cabal install --dependencies-only
cabal build
```

### Generate Parser Libraries

```bash
# Generate JavaScript library
make generate-js

# Generate TypeScript library  
make generate-ts

# Generate both
make generate-all
```

### Usage

#### JavaScript

```javascript
const { EDNParser } = require('./edn-parser.js');

const parser = new EDNParser();
const result = parser.parse('[:hello "world" #{1 2 3}]');
console.log(result);
```

#### TypeScript

```typescript
import { EDNParser, EDNValue } from './edn-parser';

const parser = new EDNParser();
const result: EDNValue = parser.parse('[:hello "world" #{1 2 3}]');

if (result.isVector()) {
  const vector = result.asVector();
  console.log('Parsed vector with', vector.length, 'elements');
}
```

## EDN Format Support

This parser supports the complete EDN specification:

- **Primitives**: `nil`, `true`, `false`
- **Numbers**: `42`, `3.14`, `-1.5e10`
- **Strings**: `"hello world"`, `"escaped \"quotes\""`
- **Characters**: `\a`, `\newline`, `\space`, `\tab`
- **Keywords**: `:keyword`, `:namespace/keyword`
- **Symbols**: `symbol`, `namespace/symbol`
- **Collections**:
  - Lists: `(1 2 3)`
  - Vectors: `[1 2 3]`
  - Maps: `{:key "value" :count 42}`
  - Sets: `#{1 2 3}`
- **Tagged values**: `#inst "2023-01-01"`, `#uuid "f47ac10b-58cc-4372-a567-0e02b2c3d479"`
- **Comments**: `; This is a comment`

## Development

### Project Structure

```
edn-parser/
├── src/
│   └── EDN/
│       ├── Types.hs          # EDN value types
│       ├── Parser.hs         # EDN parser implementation
│       └── CodeGen/
│           ├── JavaScript.hs # JS code generator
│           └── TypeScript.hs # TS code generator
├── app/
│   └── Main.hs              # Command-line interface
├── test/
│   └── Spec.hs              # Test suite
└── edn-parser.cabal         # Project configuration
```

### Running Tests

```bash
make test
# or
cabal test
```

### Example EDN Files

The generated parsers can handle complex EDN structures like:

```edn
{:users [{:id 1
          :name "Alice"
          :email "alice@example.com"
          :tags #{:admin :active}
          :created #inst "2023-01-15T10:30:00Z"}
         {:id 2
          :name "Bob" 
          :email "bob@example.com"
          :tags #{:user}
          :created #inst "2023-02-01T14:20:00Z"}]
 :metadata {:version "1.0"
            :created-by :system}}
```

## License

MIT