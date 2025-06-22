# Elang Language Specification

**Version:** 0.1.0  
**Status:** Draft  
**Audience:** Language Implementors

---

## 1. Introduction

This document provides the technical specification for the `elang` programming language. It is intended to guide the development of compilers, interpreters, and other tooling.

### 1.1. Philosophy

Elang is a statically-typed, functional programming language with a Lisp-like syntax. It is designed around three core principles:

1.  **Safety:** The type system and memory model are designed to eliminate entire classes of common bugs, such as null pointer exceptions and race conditions.
2.  **Purity:** Functions are pure by default, meaning they have no side effects. This makes code easier to reason about, test, and parallelize.
3.  **Simplicity:** The syntax is minimal and consistent, reducing cognitive load and allowing developers to focus on logic rather than language intricacies.

### 1.2. Design Goals

*   **Static Typing:** All types are known at compile time, providing strong guarantees of correctness.
*   **No Impurities:** Side effects are disallowed, enforcing a pure functional style.
*   **Expressive Generics:** Generic programming is a first-class feature for building reusable data structures.
*   **Minimal Control Flow:** Looping is achieved through recursion, simplifying the language's core.

---

## 2. Lexical Structure

### 2.1. Comments

Comments are single-line and begin with a double semicolon `;;`. They extend to the end of the line.

```elang
;; This is a comment.
(def pi 3.14) ;; This is an inline comment.
```

### 2.2. Identifiers

Identifiers can contain alphanumeric characters and the following symbols: `-`, `*`, `+`, `/`, `=`, `<`, `>`. They cannot start with a number.

*   **Valid:** `x`, `my-variable`, `list-length`, `+`, `*`
*   **Invalid:** `1st-place`

### 2.3. Literals

*   **Integer (`int`):** A sequence of digits, e.g., `123`, `0`, `42`.
*   **Float (`float`):** A sequence of digits with a decimal point, e.g., `3.14`, `0.0`.
*   **Boolean (`bool`):** `true` or `false`.
*   **Character (`char`):** A single character enclosed in single quotes, e.g., `'a'`, `'\n'`.

### 2.4. S-expressions

The fundamental syntactic unit is the S-expression (Symbolic Expression), which is either an atom (literal, identifier) or a list of other S-expressions enclosed in parentheses `()`.

---

## 3. Syntax and Semantics

### 3.1. Definitions

#### `(fun ...)` - Function Definition

Defines a function.

**Syntax:**
`(fun (name param1 :type1 ...) :return-type body-expr)`

*   If `:return-type` is omitted, it defaults to `void`. A `void` function implicitly returns, and its body must not produce a value.
*   The function body is a single expression.

**Examples:**
```elang
;; A function with an implicit void return type.
(fun (hello name :string) (print "hello" name))

;; A function that returns an integer.
(fun (add a :int b :int) :int (+ a b))
```

#### `(def ...)` - Top-level Definition

Defines a top-level constant or a named struct.

**Syntax (Constant):**
`(def name value)`
`(def name :type value)`

**Syntax (Struct):**
`(def name (struct field1 :type1 ...))`

**Examples:**
```elang
(def pi 3.14)
(def e :float 2.78)
(def int-vec2 (struct x :int y :int))
```

### 3.2. Generics

Generic structs are defined by including type parameters in the `def` form.

**Syntax:**
`(def (name T1 T2 ...) (struct field1 :T1 ...))`

**Usage:**
When using a generic struct, the type arguments must be provided. `(cons T)` is a valid type identifier inside a definition, but when used as a concrete type, it must be something like `(cons int)`.

**Examples:**
```elang
;; Definition of a generic 3D vector.
(def (vec3 T) (struct x :T y :T z :T))

;; Usage: creating a vector of floats.
(def my-vec :(vec3 float) (make-vec3 1.0 2.0 3.0))

;; Definition of the standard library cons cell.
(def (cons T) (struct e :T next :(Option (cons T))))
```

### 3.3. Control Flow

#### `(if ...)`

The sole control flow construct. It is an expression that evaluates to one of two branches.

**Syntax:**
`(if condition-expr then-expr else-expr)`

*   `condition-expr` must evaluate to a `bool`.
*   `then-expr` and `else-expr` must evaluate to the same type.

### 3.4. Local Bindings

#### `(let [...] ...)` - Simultaneous Binding

Binds variables within a lexical scope. All `binding-value` expressions are evaluated first, and then the bindings are created simultaneously. This means a binding cannot refer to another binding within the same `let` block.

**Syntax:**
`(let [(binding-form value-form) ...] body-expr)`

*   The `binding-form` can be a simple identifier for a standard variable binding or a destructuring pattern.

**Example:**
```elang
;; Destructuring a vector.
(let [((vec2 x y) (make-vector 10 20))
      (other-var 30)]
  (print x y other-var)) ;; x is 10, y is 20
```

#### `(let* [...] ...)` - Sequential Binding

Similar to `let`, but bindings are evaluated and created sequentially. A binding can refer to previous bindings in the same `let*` block.

**Syntax:**
`(let* [(binding-form-1 value-form-1) (binding-form-2 value-form-2)] body-expr)`

**Example:**
```elang
(let* [(x 10)
       (y (+ x 5))] ;; y is 15
  y)
```

---

## 4. Type System

### 4.1. Primitive Types

*   `int`: 64-bit signed integer.
*   `float`: 64-bit floating-point number.
*   `bool`: `true` or `false`.
*   `char`: A single Unicode scalar value.

### 4.2. Composite Types

#### `(Option T)`

A built-in generic type to represent an optional value. It is designed to replace `null`.
*   It has two states: a value of type `T`, or `none`.
*   Unlike structs, `(Option T)` is a trivial type and is stack-allocated.
*   A function returning `(Option int)` can return an `int` directly or `none`.

**Example:**
```elang
(fun (half-if-even n :int) :(Option int)
  (if (= (mod n 2) 0)
      (div n 2)
      none))
```

#### `struct`

A compound data type that aggregates a fixed set of named fields. All structs are non-trivial types.

### 4.3. Function Types

A function's type can be described for use in higher-order functions.

**Syntax:**
`(fun-type (arg-type ...) return-type)`

**Example:**
```elang
;; A function that takes a function as an argument.
(fun (apply-to-five f :(fun-type (int) int)) :int
  (f 5))

;; Using the higher-order function.
(def (double x :int) :int (* x 2))
(let [result (apply-to-five double)] ;; result is 10
  result)
```

### 4.4. Type Inference

Type inference should be supported for `def` and local bindings `(let, let*)`. Function signatures require explicit type annotations.

---

## 5. Memory Model

The memory model is simple and deterministic.

*   **Stack Allocation (Trivial Types):**
    *   `int`, `float`, `bool`, `char`
    *   `(Option T)` (where T is any type)
    *   These types are passed by value.

*   **Heap Allocation (Non-trivial Types):**
    *   `struct` instances are always allocated on the heap.
    *   Structs are passed by reference.
    *   Memory management is assumed to be handled by a garbage collector (GC).

---

## 6. Modules and Linking

### 6.1. Exports `(provides ...)`

A file exposes definitions to other files using a single `(provides ...)` statement at the top level.

**Syntax:**
`(provides name1 name2 ...)`

### 6.2. Imports `(require "...")`

A file imports definitions from another file using `(require "...")`.

**Syntax:**
`(require "path/to/file.elang")`

### 6.3. Linking Semantics

*   When a file is required, only the definitions it `provides` are imported into the current scope.
*   Top-level code in a required file is **not** executed at import time. This prevents side effects from module loading.

---

## 7. Standard Library

The standard library provides a minimal set of core data structures and functions.

### 7.1. Core Types

#### `(cons T)`

The fundamental generic list structure.
`(def (cons T) (struct e :T next :(Option (cons T))))`

#### `string`

A string is defined as a list of characters.
`(def string (cons char))`

### 7.2. Core Functions

A set of built-in functions (e.g., `+`, `-`, `*`, `/`, `print`, `mod`, `div`, `=`) are assumed to be available in the global scope. Implementors should provide these primitives.