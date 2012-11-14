What is it?
===========

A concurrent, lock-free, ordered set data type based on skip lists (which in
turn utilise `atomicModifyIORef`).


Status
======

Passes all single-threaded QuickCheck test cases.


Installation
============

TODO


Usage
=====

TODO


To Do
=====

* Multi-threaded test cases
* Benchmarks (esp. scalability)
* Make OrderedSet instances of Functor, Foldable, etc.
* Haddock documentation
* Cabal package


License
=======

BSD3


References
==========

* Herlihy, Shavit: The Art of Multiprocessor Programming
