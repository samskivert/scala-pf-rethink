Scala Partial Function Rethink
------------------------------

This is some fiddling around to better understand the performance
characteristics of different approaches to PartialFunction, with the aim of
avoiding the double evaluation of the guards that happens with `if
(pf.isDefinedAt(x)) pf(x)` style code.

Run the benchmarks with `sbt run`.
