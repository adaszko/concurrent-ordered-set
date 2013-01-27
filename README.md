What is it?
===========

A concurrent, lock-free, ordered set data type based on skip lists (which in
turn utilise `atomicModifyIORef`).


Status
======

Passes all QuickCheck test cases.


Installation
============

Assuming you have GHC already installed:

    $ cabal configure --enable-tests
    $ cabal build
    $ cabal test
    $ cabal install


Usage
=====

```haskell
import Data.Concurrent.OrderedSet

main = do
  oset <- fromList [1..3]
  delete 2 oset
  result <- toList oset
  putStrLn $ show result
```


To Do
=====

* Haddock documentation
* Enable -Wall and clean resulting warnings


License
=======

BSD3


References
==========

* [Multicore Programming in Haskell](http://www.infoq.com/presentations/Multicore-Programming-in-Haskell)
* Herlihy, Shavit: The Art of Multiprocessor Programming
