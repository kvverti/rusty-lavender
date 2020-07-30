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
# values
def vec => { 1; 2; 3 }

# functions
def repeat a => { a; a }

# infix operators
def (.)' f g a => f (g a)

# lambda expressions
def id => lam a. a

# type annotations
def const: 'a -> (for b. b -> 'a);
  a _ => a

# piecewise functions
def optmap f
  ; (Some a) => Some (f a)
  ; _ => None
  
# type aliases
type Binary a => a -> a -> a

# type classes
class Eq a {
  def eq: a -> a -> Bool
}

def '(=) => Eq::eq
```

Infix operators have no precedence, therefore mixing infix operators is not allowed.
```lavender
def wrong => 1 + 2 * 3
def right => 1 + (2 * 3)
```

Evaluation is lazy, making constructing infinite data structures simple.
```lavender
def repeat x => x +: repeat x
```
