# F# ↔ Haskell: syntax & concepts

## basics

| Concept             | F#                                | Haskell                         |
| ------------------- | --------------------------------- | ------------------------------- |
| single-line comment | `// ...`                          | `-- ...`                        |
| multi-line comment  | `(* ... *)`                       | `{- ... -}`                     |
| file/module header  | `module M` / `namespace N`        | `module M where`                |
| import              | `open X.Y`                        | `import X.Y`                    |
| entry point         | `let main _ = ...` (console apps) | `main :: IO ()`<br>`main = ...` |
| indentation         | significant (tabs/spaces)         | significant (layout rule)       |

## values, functions, application

|                     | F#                                    | Haskell                                       | 
| ------------------- | ------------------------------------- | --------------------------------------------- | 
| value               | `let x = 42`                          | `x = 42`                                      | 
| type annotation     | `let x: int = 42`                     | `x :: Int`<br>`x = 42`                        | 
| function            | `let add x y = x + y`                 | `add x y = x + y`                             | 
| annotate function   | `let add (x:int) (y:int) : int = ...` | `add :: Int -> Int -> Int`<br>`add x y = ...` | 
| application         | `add 1 2`                             | `add 1 2`                                     | 
| partial application | `let inc = add 1`                     | `inc = add 1`                                 | 
| composition         | `let h = f >> g` / `let h = g << f`   | `h = g . f`                                   | 
| forward pipe        | `x \|> f`                             | `x & f` (needs: import Data.Function ((&)))   | 
| backward apply      | `x <\| f`                             | `x $ f` (needs: import Data.Function ((&)))   | 
| sections            | `(+1)`; `(1+)`             | `(1+)`, `(+1)`                     | 

## bindings & scope

|                   | F#                     | Haskell                                      |
| ----------------- | ---------------------- | -------------------------------------------- |
| local binding     | `let x = ... in expr`  | `let x = ... in expr`                        |
| `where` clauses   | N/A                    | `f x = y`<br>`  where y = ...`               |
| recursive binding | `let rec fact n = ...` | all top-level `let` are recursive by default |

## conditionals & pattern matching

|          | F#                      | Haskell            |
| -------- | ----------------------- | ------------------ |
| if       | `if cond then a else b` | same               |                
| match    | `match v with \| A -> ... \| B x -> ...` | `case v of A -> ...; B x -> ...` |
| guards   | `match x with \| x when x>0 -> ...` | `f x \| x>0 = ...` *(guards on equations)* |                                  |
| wildcard | `_`                     | `_`                |                                            |                                  |

## tuples, lists, arrays, strings

|           | F#                              | Haskell                           |
| --------- | ------------------------------- | --------------------------------- |
| tuple     | `(1, "a")`                      | `(1, "a")`                        |
| list      | `[1;2;3]`                       | `[1,2,3]`                         |
| cons      | `1 :: [2;3]`                    | `1 : [2,3]`                       |
| head/tail | `List.head xs` / `List.tail xs` | `head xs` / `tail xs`             |
| array     | `[\| 1;2 \|]` | `Array` via lib; use lists or `vector` |
| string    | `"hi"` is `string`              | `"hi"` is `[Char]`; prefer `Text` |

## records & DUs (ADTs)

|              | F#                                       | Haskell                                               |
| ------------ | ---------------------------------------- | ----------------------------------------------------- |
| record type  | `type Person = { Name:string; Age:int }` | `data Person = Person { name :: String, age :: Int }` |
| record value | `{ Name="A"; Age=1 }`                    | `Person { name="A", age=1 }`                          |
| update       | `{ p with Age=2 }`                       | `p { age = 2 }`                                       |
| union / sum  | `type Shape = Circle of r:int \| Rect of w:int * h:int` | `data Shape = Circle Int \| Rect Int Int` |
| deriving     | `(* auto ToString via printf *)`         | `deriving (Show, Eq, Ord)`                            |

## options, results, eithers

|               | F#                          | Haskell                      |
| ------------- | --------------------------- | ---------------------------- |
| option/maybe  | `Some 1` / `None`           | `Just 1` / `Nothing`         |
| result/either | `Ok x` / `Error e`          | `Right x` / `Left e`         |
| map           | `Option.map f`              | `fmap f`                     |
| bind          | `Option.bind f`             | `>>=` (for `Maybe`/`Either`) |
| default       | `Option.defaultValue d opt` | `fromMaybe d m`              |

## higher-order, folds, comprehensions

|           | F#                                 | Haskell                                     |
| --------- | ---------------------------------- | ------------------------------------------- |
| map       | `List.map f xs`                    | `map f xs`                                  |
| filter    | `List.filter p xs`                 | `filter p xs`                               |
| fold      | `List.fold f s xs`                 | `foldl' f s xs` *(strict)* / `foldr f z xs` |
| list comp | `seq { for x in xs do yield x+1 }` | `[x+1 \| x <- xs]` |

## laziness & strictness

|             | F# (strict)                      | Haskell (lazy by default)                                           |
| ----------- | -------------------------------- | ------------------------------------------------------------------- |
| lazy value  | `lazy (exp)`;                    | default lazy; use `seq`/`deepseq`/`bang` patterns `!x` for strictness |
| strict fold | `List.fold` is strict in state   | prefer `foldl'` for large lists                                     |

## effects & monads

|                   | F#                                 | Haskell                                             |
| ----------------- | ---------------------------------- | --------------------------------------------------- |
| computation expr  | `async { ... }`, `task { ... }`    | `do`-notation                                       |
| IO                | `System.IO`, side effects anywhere | `IO` monad: `main :: IO ()`                         |
| monad ops         | CE `let!`, `return`                | `>>=`, `>>`, `pure`, `return`                       |
| reader/state      | custom CE or libraries             | `Reader`, `State`, `Except` (`mtl`, `transformers`) |
| async/concurrency | `Async`, `Task`                    | `async`, `STM` (`atomically`, `TVar`)               |

### small monad example

**F# (option CE)**:

```fsharp
let addParsed a b =
  option {
    let! x = System.Int32.TryParse a |> Option.ofTry
    let! y = System.Int32.TryParse b |> Option.ofTry
    return x + y
  }
```

**Haskell (Maybe + do)**:

```haskell
readInt :: String -> Maybe Int
readInt s = case reads s of [(n,"")] -> Just n; _ -> Nothing

addParsed :: String -> String -> Maybe Int
addParsed a b = do
  x <- readInt a
  y <- readInt b
  pure (x + y)
```

## typeclasses vs interfaces

|                     | F#                          | Haskell                                     |
| ------------------- | --------------------------- | ------------------------------------------- |
| ad-hoc polymorphism | interfaces, SRTPs           | **typeclasses**                             |
| define              | interface + impls           | `class Show a where show :: a -> String`    |
| implement           | `type T with interface ...` | `instance Show T where show ...`            |
| use                 | method dispatch             | constraints: `foo :: Show a => a -> String` |

## newtype & type aliases

|                | F#                      | Haskell                                              |
| -------------- | ----------------------- | ---------------------------------------------------- |
| alias          | `type OrderId = string` | `type OrderId = String` *(alias)*                    |
| zero-cost wrap | often single-case DU    | `newtype OrderId = OrderId String` (no runtime cost) |

## numeric & overloading

|                      | F#              | Haskell                               |
| -------------------- | --------------- | ------------------------------------- |
| operator overloading | limited / SRTPs | via typeclasses (`Num`, `Fractional`) |
| from int/float       | casts           | `fromIntegral`, `realToFrac`          |

## errors & exceptions

|            | F#                       | Haskell                                               |
| ---------- | ------------------------ | ----------------------------------------------------- |
| exceptions | `try ... with ex -> ...` | `catch` in `IO`; prefer `Either/ExceptT` in pure code |
| fail fast  | throw                    | `error "msg"` *(avoid in production)*                 |

## modules & names

|             | F#                         | Haskell                                |
| ----------- | -------------------------- | -------------------------------------- |
| module      | `module M` or `module rec` | `module M (exports) where`             |
| open/import | `open X`                   | `import X (..)` / `qualified`          |
| qualified   | `open type`/aliases        | `import qualified M as X` then `X.foo` |

## deriving & generics

|               | F#                        | Haskell                                         |
| ------------- | ------------------------- | ----------------------------------------------- |
| auto eq/show  | structural eq, `ToString` | `deriving (Eq, Ord, Show, Generic)`             |
| generic prog. | SRTPs, `MemberConstraint` | `GHC.Generics`, `DerivingVia`, `DeriveAnyClass` |

## quick JSON example

**F# (System.Text.Json)**

```fsharp
type Person = { name: string; age: int }
let json = JsonSerializer.Serialize { name="Ada"; age=37 }
```

**Haskell (aeson)**

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson
import GHC.Generics (Generic)

data Person = Person { name :: String, age :: Int }
  deriving (Show, Generic)

instance FromJSON Person
instance ToJSON   Person

json = encode (Person "Ada" 37)
```

## IO & files

**F#**

```fsharp
let lines = System.IO.File.ReadAllLines "a.txt"
```

**Haskell**

```haskell
import qualified Data.Text.IO as T
txt <- T.readFile "a.txt"
```

## tips moving F# → Haskell

* **Laziness by default:** watch space leaks; use `seq`, `deepseq`, `foldl'`, bang patterns.
* **Typeclasses everywhere:** numeric ops, equality, printing are constrained (`Eq`, `Show`, `Num`).
* **All top-level bindings are recursive**; use `where`/`let` elegantly.
* **Lists are linked lists**; reach for `Vector`/`Text`/`ByteString` for perf.
* **`newtype`** is your friend for domain safety with zero overhead.


## Haskell Advanced features: 
    `GADTs`, `DataKinds`, `TypeFamilies`, `GeneralizedNewtypeDeriving`.
## F# (with FSharpPlus) vs Haskell - Effects & Monads Cheatsheet

### Imports

**F#:**
- nuget: FSharpPlus
- `open FSharpPlus`
- `open FSharpPlus.Data`

**Haskell:**
- base + transformers + mtl + containers (as needed)

### Functor / Applicative / Monad

**F#:**
```fsharp
open FSharpPlus
let plus1 = map ((+) 1) (Some 1)        // Functor: map / <!>
let apEx  = apply (Some (+3)) (Some 4)  // Applicative: apply / <*>
let bindEx = Option.bind (fun x -> Some (x*2)) (Some 3) // Monad: bind / >>=
```

**Haskell:**
```haskell
fmap ((+) 1) (Just 1)
(Just (+3)) <*> (Just 4)
Just 3 >>= \x -> Just (x*2)
```

### Standard Aliases

**F#:**
- `map` == `<!>`
- `apply` == `<*>`
- `bind` == `>>=` (from F#+)

**Haskell:**
- `fmap` == `<$>`
- `(<*>)` Applicative apply
- `(>>=)` Monad bind

### Maybe / Option

**F#:**
```fsharp
open FSharpPlus
let parseInt (s:string) =
  match System.Int32.TryParse s with true, n -> Some n | _ -> None

let addParsed a b =
  maybe {
    let! x = parseInt a      // maybe CE is in F#+
    let! y = parseInt b
    return x + y
  }
```

**Haskell:**
```haskell
readInt s = case reads s of [(n,"")] -> Just n; _ -> Nothing
addParsed a b = do x <- readInt a; y <- readInt b; pure (x + y)
```

### Result / Either

**F#:**
```fsharp
open FSharpPlus
type Err = string
let divide x y = if y=0 then Error "div by zero" else Ok (x / y)

let calc a b =
  result {
    let! x = divide a b
    return x * 10
  }
```

**Haskell (Either String):**
```haskell
divide x y = if y==0 then Left "div by zero" else Right (x `div` y)
calc a b = do x <- divide a b; pure (x * 10)
```

### Reader

**F#:**
```fsharp
open FSharpPlus.Data
type Cfg = { factor:int }

let multByCfg : Reader<Cfg,int> =
  reader {
    let! cfg = ask ()               // ask :: Reader<Cfg, Cfg>
    return cfg.factor * 3
  }
// runReader multByCfg { factor = 7 }  // -> 21
```

**Haskell:**
```haskell
newtype Reader r a = Reader { runReader :: r -> a }
multByCfg = do cfg <- ask; pure (factor cfg * 3)
runReader multByCfg (Cfg 7)
```

### State

**F#:**
```fsharp
open FSharpPlus.Data
let inc : State<int,unit> =
  state {
    let! n = get ()
    do! put (n+1)
  }
// runState inc 10  // -> ((), 11)
```

**Haskell:**
```haskell
inc = do n <- get; put (n+1)
```

### Writer (logging)

**F#:**
```fsharp
open FSharpPlus.Data
let step : Writer<List<string>, int> =
  writer {
    do! tell ["start"]
    return 42
  }
// runWriter step  // -> (42, ["start"])
```

**Haskell:**
```haskell
step = do tell ["start"]; pure 42
```

### Monad stacks (Reader + State + Result/Either)

**F# with F#+ transformers:**
```fsharp
type App<'a> = ReaderT<Cfg, StateT<int, ResultT<Err, 'a>>>

let runApp (m: App<'a>) cfg st =
  m |> ReaderT.run cfg |> StateT.run st |> ResultT.run

let program : App<int> =
  monad {
    let! cfg = ReaderT.ask ()
    do! StateT.modify ((+) 1)
    if cfg.factor < 0 then
       return! ResultT.throw "bad cfg"
    let! s = StateT.get ()
    return cfg.factor + s
  }
// runApp program { factor = 2 } 10  -> Ok (13, 11)
```

**Haskell (mtl):**
```haskell
type App a = ReaderT Cfg (StateT Int (ExceptT Err IO)) a   -- or without IO
program = do
  cfg <- ask
  modify (+1)
  when (factor cfg < 0) (throwError "bad cfg")
  s <- get
  pure (factor cfg + s)
```

### Traversable / sequence / traverse

**F# (F#+):**
```fsharp
let parsed : Option<int list> = traverse parseInt ["1";"2";"x"]  // -> None
let lifted : list<option<int>> = sequence [Some 1; None; Some 3]  // -> None overall
```

**Haskell:**
```haskell
traverse readInt ["1","2","x"]   -- :: Maybe [Int]
sequence [Just 1, Nothing, Just 3]
```

> **Note:** F#+ traverse/sequence are generic over many containers, like Haskell.

### Alternative / Plus (choice, failure)

**F#:**
```fsharp
open FSharpPlus.Control
let pickFirst : option<int> = (None <|> Some 42)              // <|> from Alternative
let guarded = guard (5 > 3) *> Some "ok"                      // guard + *> in F#+
```

**Haskell:**
```haskell
pickFirst = Nothing <|> Just 42
guarded   = guard (5 > 3) *> Just "ok"
```

### Validation (Applicative accumulation)

**F# (F#+ Validation accumulates errors Applicatively):**
```fsharp
open FSharpPlus.Data
let validatePositive x = if x > 0 then Success x else Failure ["not positive"]
let validateSmall   x = if x < 10 then Success x else Failure ["too large"]

let combined =
  let (<*>) = Validation.apply   // ensure Applicative
  Validation.map2 (fun a b -> a + b) (validatePositive 5) (validateSmall 12)
// -> Failure ["too large"]
```

**Haskell (Data.Validation from 'validation' pkg):**
```haskell
validatePositive x = if x>0 then Success x else Failure ["not positive"]
validateSmall   x = if x<10 then Success x else Failure ["too large"]
combined = (+) <$> validatePositive 5 <*> validateSmall 12
-- Failure ["too large"]
```

### Async / IO

**F#:**
```fsharp
// Async: built-in; Task: .NET
let ioExample =
  async {
    do! Async.Sleep 10
    return 123
  }
```

**Haskell:**
```haskell
import Control.Concurrent (threadDelay)
ioExample = do threadDelay 10000; pure 123 :: IO Int
```

### Lifting / Hoisting helpers

**F# (F#+):**
```fsharp
let liftReader (x: Reader<Cfg,int>) : ReaderT<Cfg, Result<int, _>> =
  ReaderT (fun r -> Ok (Reader.run x r))
```

**Haskell:**
```haskell
-- liftReader = hoist (withReaderT ...) or use 'lift' when inside a transformer stack
```

### Common Operators Summary

**F# (F#+):**
- `<!>` : Functor map
- `<|>` : Alternative choice
- `<&>` : flipped map
- `>>=` : bind (generic)
- `*>` , `<*` : sequence, keep right/left
- `traverse` / `sequence`
- `ask`, `local`, `reader {..}`
- `state {..}`, `get`/`put`/`modify`
- `writer {..}`, `tell`/`listen`
- `ResultT`/`OptionT`/`ReaderT`/`StateT`/`WriterT`

**Haskell:**
- `<$>` , `<*>` , `>>=` , `=<<`
- `<|>` , `empty`
- `&` (Data.Function) -- forward pipe analogue
- `traverse` / `sequence` / `traverse_`
- `ask`, `local` (Reader), `get`/`put`/`modify` (State), `tell` (Writer)
- `ExceptT`/`MaybeT`/`ReaderT`/`StateT`/`WriterT`
