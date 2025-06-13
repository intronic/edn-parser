// Test script for the generated JavaScript EDN parser
const fs = require('fs');
const { EDNParser, EDNValue } = require('../edn-parser.js');

function runTests() {
  const parser = new EDNParser();
  
  console.log('=== EDN Parser JavaScript Test ===\n');
  
  // Test 1: Simple values
  console.log('Test 1: Simple values');
  testParse(parser, 'nil');
  testParse(parser, 'true');
  testParse(parser, 'false');
  testParse(parser, '42');
  testParse(parser, '"hello world"');
  testParse(parser, ':keyword');
  testParse(parser, 'symbol');
  console.log('');
  
  // Test 2: Collections
  console.log('Test 2: Collections');
  testParse(parser, '[1 2 3]');
  testParse(parser, '(+ 1 2 3)');
  testParse(parser, '#{:a :b :c}');
  testParse(parser, '{:name "Alice" :age 30}');
  console.log('');
  
  // Test 3: Complex nested structure
  console.log('Test 3: Complex structure');
  const complexEdn = `{:users [{:id 1 :name "Alice" :tags #{:admin :active}}]
                       :config {:enabled true :count 42}}`;
  testParse(parser, complexEdn);
  console.log('');
  
  // Test 4: Load and parse sample file
  console.log('Test 4: Sample EDN file');
  try {
    const sampleContent = fs.readFileSync('examples/sample.edn', 'utf8');
    console.log('Loading sample.edn...');
    const result = parser.parse(sampleContent);
    console.log('✓ Successfully parsed sample.edn');
    console.log('Type:', result.type);
    if (result.type === 'map') {
      console.log('Keys in root map:', Array.from(result.value.keys()).map(k => 
        k.type === 'keyword' ? ':' + k.value : k.value
      ));
    }
  } catch (error) {
    console.log('✗ Error parsing sample.edn:', error.message);
  }
  console.log('');
  
  console.log('=== Tests completed ===');
}

function testParse(parser, input) {
  try {
    const result = parser.parse(input);
    console.log(`✓ "${input}" -> ${result.type}: ${formatValue(result)}`);
  } catch (error) {
    console.log(`✗ "${input}" -> Error: ${error.message}`);
  }
}

function formatValue(ednValue) {
  switch (ednValue.type) {
    case 'nil': return 'nil';
    case 'bool': return ednValue.value.toString();
    case 'number': return ednValue.value.toString();
    case 'string': return `"${ednValue.value}"`;
    case 'keyword': return `:${ednValue.value}`;
    case 'symbol': return ednValue.value;
    case 'list': return `(${ednValue.value.map(formatValue).join(' ')})`;
    case 'vector': return `[${ednValue.value.map(formatValue).join(' ')}]`;
    case 'set': return `#{${Array.from(ednValue.value).map(formatValue).join(' ')}}`;
    case 'map': return `{${Array.from(ednValue.value.entries()).map(([k,v]) => `${formatValue(k)} ${formatValue(v)}`).join(' ')}}`;
    default: return ednValue.value.toString();
  }
}

// Run the tests
runTests();