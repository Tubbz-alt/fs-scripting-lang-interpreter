# Scripting language

## Supports:
  * Atomics: `int`, `bool`, `string`
  * Conditions: if-statement and if-else-statement
  * Lexial scoping within codeblocks
  * Comparision expresions: `==`, `!=`, `<`, `>`
  * Boolean algabra: `&&`, `||`
  * Echo statement

## Examples
```
let x = 2
let y = 1

if x == y then {
  echo "yes"
} else {
  echo "no"
}

echo x
```

```
let x = 2
let y = 1

if x == y || x == 2 then {
  echo "yes"
} 

echo x
```