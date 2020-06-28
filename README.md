# Rusty-Lavender
The third iteration of the Lavender programming language, implemented in Rust.

Lavender is a pure functional, lazy, (eventally) typed programming language. Its basic building blocks are intrinsic values and functions. Features include:

- Fundamental types such as `Int`, `Float`, `String`, `Unit`
- Lazy evaluation of functions
- Product and sum types (NYI)
- Pattern matching (NYI)
- Type classes (NYI)
- Higher kinded types (NYI)

Lavender is written using a Haskell-like syntax and is compiled to a bytecode that is run using an interpreter.

```lavender
def addOneInt(a: Int) = 1 + a

data Rectangle (width: Int) (height: Int)

def ofWidthThree = Rectangle 3

def widthThreeHeightFive = ofWidthThree (addOneInt 4)

def five = widthThreeHeightFive::height
```
