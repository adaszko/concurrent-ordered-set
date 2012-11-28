What is it?
===========

A concurrent, lock-free, ordered set data type based on skip lists (which in
turn utilise `atomicModifyIORef`).


Status
======

Passes all single-threaded QuickCheck test cases.  Fails multi-threaded ones.


Installation
============

TODO


Usage
=====

TODO


To Do
=====

* Benchmarks (esp. scalability)
* Haddock documentation
* Cabal package


License
=======

BSD3


References
==========

* Herlihy, Shavit: The Art of Multiprocessor Programming
