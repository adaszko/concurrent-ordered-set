What is it?
===========

A concurrent, ordered map data type based on skip lists.


Status
======

At this point, this is still a set, not a map.  Passes all single-threaded
QuickCheck test cases.


To Do
=====

* Multi-threaded test cases
* Benchmarks (esp. scalability)
* Make Map instances of Functor, Foldable, etc.
* Haddock documentation
* Cabal package


License
=======

BSD3


References
==========

* Herlihy, Shavit: The Art of Multiprocessor Programming
