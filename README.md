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

    $ runhaskell Setup.hs configure --enable-tests
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs test
    $ runhaskell Setup.hs install


Usage
=====

TODO


To Do
=====

* Benchmarks (esp. scalability)
* Haddock documentation
* Enable -Wall and clean resulting warnings


License
=======

BSD3


References
==========

* Herlihy, Shavit: The Art of Multiprocessor Programming
