# What is it?

A concurrent, lock-free, ordered set data type based on skip lists (which in
turn utilise `atomicModifyIORef`).


# Status

Passes all QuickCheck test cases.  Here are some performance measurements against `Data.Set`:

## Absolute

![insert](https://github.com/adaszko/concurrent-ordered-set/raw/master/artifacts/insert-comparison.svg "insert")
![contains](https://github.com/adaszko/concurrent-ordered-set/raw/master/artifacts/contains-comparison.svg "contains")
![delete](https://github.com/adaszko/concurrent-ordered-set/raw/master/artifacts/delete-comparison.svg "delete")


## Scalability

![insert](https://github.com/adaszko/concurrent-ordered-set/raw/master/artifacts/insert-scalability.svg "insert")
![contains](https://github.com/adaszko/concurrent-ordered-set/raw/master/artifacts/contains-scalability.svg "contains")
![delete](https://github.com/adaszko/concurrent-ordered-set/raw/master/artifacts/delete-scalability.svg "delete")


# Installation

Assuming you have GHC already installed:

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test
    $ cabal install


# Usage

```haskell
import Data.Concurrent.OrderedSet

main = do
  oset <- fromList [1..3]
  delete 2 oset
  result <- toList oset
  putStrLn $ show result
```


# License

BSD3


# References

* [Multicore Programming in Haskell](http://www.infoq.com/presentations/Multicore-Programming-in-Haskell)
* Herlihy, Shavit: The Art of Multiprocessor Programming
