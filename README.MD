# Scripting language

A simple scripting language writing in F# with FsLexYacc. Main features are:
* Collections
* Boolean algabra
* Controlflow: `if`, `if-else`, `foreach`

## Variables

Variables are created with the `let` keyword:
```
let x = 5
```
Variables are mutable:
```
let x = 5
x = 6
```
String, bool and int are the supported primitive datatypes:
```
let string = "a string"
let int = 1
let bool = true
```

## Scoping
This language uses lexial scoping. This will print 4 and 2:
```
let x = 1
if true then {
  x = 2
  let x = 3
  x = 4
  echo x
}
echo x
```

## Echo

with `echo` you can print any variable:
```
let x = 1
echo x
echo "hi"
```

## Collections

Collections are implemented as F# lists. They are created with `[` and `]`
```
let col = [1,2,3]
```
You can mix types in one collection:
```
let col = [1, "1", true]
```
Items are accesed by index:
```
let col = [1,2,3]
let snd = x[1]
x[1] = 5
```

## Iterations

Lists can be iterated with the `foreach` keyword:
```
let col = [1,2,3]
foreach item in col {
  echo item
}
```

## Comparision

Values can be compared with `!=`, `==`, `<` en `>`. You can only compare variables with the same types. Types will not chance at runtime. Boolean algabra is implemented with `||` and `&&`

## Conditions

Flowcontrol is implemented with if-statements and if-else-statements. For example:
```
if 5 == 4 then {
  echo "shouldn't happed"
}
```

```
if 5 == 4 then {
  echo "shouldn't happed"
} else {
  echo "should happen"
}
```