# Scala Partial Function Rethink

This is some fiddling around to better understand the performance
characteristics of different approaches to PartialFunction, with the aim of
avoiding the double evaluation of the guards that happens with `if
(pf.isDefinedAt(x)) pf(x)` style code.

Run the benchmarks with `sbt run`.

## Approaches

The shootout involves the following four culprits:

### PartialFunction

    trait PartialFunction[-A, +B] extends (A => B) {
      def isDefinedAt(x: A): Boolean
    }

This ends up being used in the following manner: `if (pf.isDefinedAt(x)) pf(x)`
which results in double execution of any match guards and extractors due to
execution of `isDefinedAt` as well as `apply`. It also introduces the
possibility that the state relied upon by the guards changes between the two
calls, yielding an unexpected `MatchError`.

### PartFuncPlus

    trait PartFuncPlus[-A, +B] extends (A => B) {
      def isDefinedAt(x: A): Boolean

      def apply(x :A) = applyOrElse(x, (_ :A) => throw new MatchError)

      def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
        mapOrElse(x, identity[B1], default)

      def mapOrElse[A1 <: A, B1 >: B, C](x :A1, f: B1 => C, default: A1 => C): C

      def mapIf[A1 <: A, B1 >: B](x :A1, f: B1 => Unit): Unit = 
        mapOrElse(x, f, PartFuncPlus.constUnit)
    }

The compiler implements the `mapOrElse` method (instead of `apply`), which can
be used to conditionally execute code if the function is defined, avoiding
duplicate evaluation

### SemiFunc

    trait SemiFunc[-A, +B] extends (A => B)
    {
      def missingCase(x: A): B = throw SemiFunc.undefined

      def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = try {
        apply(x)
      } catch {
        case ex: SemiFunc.UndefinedError => default(x)
      }

      def mapOrElse[A1 <: A, B1 >: B, C](x :A1, f: B1 => C, default: A1 => C): C = try {
        f(apply(x))
      } catch {
        case ex: SemiFunc.UndefinedError => default(x)
      }

      def mapIf[A1 <: A, B1 >: B](x :A1, f: B1 => Unit): Unit = mapOrElse(x, f, SemiFunc.constUnit)
    }

    object SemiFunc
    {
      final class UndefinedError extends Error
      private val undefined = new UndefinedError
      private val constUnit = (a: Any) => ()
    }

This uses Martin's "`Function1` + `missingCase` + magical creation of subclass
by compiler" idea. The compiler generates an `apply` implementation as usual,
but `missingCase` can be caused to throw an exception to divert to alternative
behavior when a function is not defined. In this case, I have added a general
purpose `mapOrElse` method so that callers do not need to understand the
internals of `SemiFunc.UndefinedError` (at the cost of some indirection).

### Collector

    trait Collector[-A, +B] extends (A => B)
    {
      def missingCase(x: A): B = throw Collector.undefined
    }

    object Collector
    {
      final class UndefinedError extends Error
      private val undefined = new UndefinedError
    }

This is a less general `SemiFunc` that allows `collect` to avoid `mapOrElse` by
understanding the internals of `UndefinedError`. (See the `collect`
implemention below.)

## Traversable.collect

The basic comparison I wanted to make was the relative performance of
`Traversable.collect` on the above approaches. The test code for each approach
is illustrative:

### PartialFunction

    def collect[A,B] (data :Traversable[A], pf :PartialFunction[A,B]): Traversable[B] = {
      val b = collection.mutable.Buffer[B]()
      for (x <- data) if (pf.isDefinedAt(x)) b += pf(x)
      b
    }

### PartFuncPlus

    def collect[A,B] (data :Traversable[A], pf :PartFuncPlus[A,B]): Traversable[B] = {
      val b = collection.mutable.Buffer[B]()
      for (x <- data) pf.mapIf(x, b.+=)
      b
    }

### SemiFunc

    def collect[A,B] (data :Traversable[A], pf :SemiFunc[A,B]): Traversable[B] = {
      val b = collection.mutable.Buffer[B]()
      for (x <- data) pf.mapIf(x, b.+=)
      b
    }

### Collector

    def collect[A,B] (data :Traversable[A], pf :Collector[A,B]): Traversable[B] = {
      val b = collection.mutable.Buffer[B]()
      for (x <- data) {
        try { b += pf(x) }
        catch { case ex: Collector.UndefinedError => /* nada */ }
      }
      b
    }

I also parameterized the test by two additional dimensions:

1. The relative expense of the guard/extractor function.
2. The ratio of defined to undefined calls of the partial function (which I
   term hit and miss in the test output).

The three partial functions are listed below, along with a sample run of the
benchmark. The three functions turn out to be not wildly different in
performance, all on the relatively cheap side, which should help to avoid
drowning the performance differences of the different approaches in the
performance overhead of double execution of the gaurds, or not.

The titles below are the actual implementations for `PartialFunction`. The
implementations for `PartFuncPlus`, `SemiFunc`, and `Collector` are
semantically the same, but differ in technical details. See the source for
further details.

The reported results were obtained in the following environment:

    Linux yonami 2.6.35-24-generic #42-Ubuntu SMP Thu Dec 2 02:41:37 UTC 2010 x86_64 GNU/Linux

    java version "1.6.0_21"
    Java(TM) SE Runtime Environment (build 1.6.0_21-b06)
    Java HotSpot(TM) 64-Bit Server VM (build 17.0-b16, mixed mode)

    Scala code runner version 2.8.0.final -- Copyright 2002-2010, LAMP/EPFL

### LT0 `{ case x if (java.lang.Integer.parseInt(str) < 0) => x }`

    [info]       PartialFunction   PartFuncPlus       SemiFunc      Collector
    [info]   Mostly hit   841098   575510 0.68x   622665 0.74x   777801 0.92x
    [info]        50/50   589078   480139 0.82x   509928 0.87x  1405985 2.39x
    [info]  Mostly miss   459286   446247 0.97x   464476 1.01x  2062160 4.49x
    [info]   Total (<0)  1889462  1501896 0.79x  1597069 0.85x  4245946 2.25x

### Left `{ case Left(left) => left }`

    [info]       PartialFunction   PartFuncPlus       SemiFunc      Collector
    [info]   Mostly hit   311820   340649 1.09x   693292 2.22x   500736 1.61x
    [info]        50/50   242685   285778 1.18x  1434926 5.91x  1177965 4.85x
    [info]  Mostly miss   132052   202161 1.53x  2146201 16.25x  1812882 13.73x
    [info] Total (Left)   686557   828588 1.21x  4274419 6.23x  3491583 5.09x

### List `{ case h :: t => h }`

    [info]       PartialFunction   PartFuncPlus       SemiFunc      Collector
    [info]   Mostly hit   304579   313041 1.03x   632337 2.08x   506471 1.66x
    [info]        50/50   242419   265104 1.09x  1313604 5.42x  1178749 4.86x
    [info]  Mostly miss   129110   182658 1.41x  1958612 15.17x  1796654 13.92x
    [info] Total (List)   676108   760803 1.13x  3904553 5.78x  3481874 5.15x

### Discussion

It seems that the overhead of throwing an exception is not being optimized away
by Hotspot in most cases (strangely, it is for `SemiFunc` in the first case
only). Furthermore, even the non-excepting `PartFuncPlus` is not out-performing
plain old `PartialFunction` except in the case of the most expensive guard
(which involves converting a string to an integer).

However, there are various issues complicating the benchmark as is. For one, a
tremendous amount of garbage is being generated and collected during the test.
This stems from the test itself (`collect` creates and populates buffers) as
well as closure creation (`b.+=`). Earlier versions of the test also created a
lot of garbage due to boxing and unboxing, which I managed to eliminate.

## collectSumLen

To try to isolate the essential performance differences of the three methods, I
created another test that avoids generating garbage. Instead of collecting
strings into a buffer, `collectSumLen` simply sums the lengths of the strings
for which the partial function returns a result. It also specializes on `Array`
to avoid `foreach` and its associated closure creation.

### PartialFunction

    def collectSumLen[A] (data :Array[A], pf :PartialFunction[A,String]): Int = {
      var sum = 0
      var ii = 0
      while (ii < data.length) {
        val x = data(ii)
        if (pf.isDefinedAt(x)) sum += pf(x).length
        ii += 1
      }
      sum
    }

### PartFuncPlus

    def collectSumLen[A] (data :Array[A], pf :PartFuncPlus[A,String]): Int = {
      var sum = 0
      var ii = 0
      while (ii < data.length) {
        sum += pf.mapOrElse(data(ii), strLen, constZero)
        ii += 1
      }
      sum
    }
    private val strLen = (x :String) => x.length
    private val constZero = (_ :Any) => 0

### SemiFunc

    def collectSumLen[A] (data :Array[A], pf :SemiFunc[A,String]): Int = {
      var sum = 0
      var ii = 0
      while (ii < data.length) {
        sum += pf.mapOrElse(data(ii), strLen, constZero)
        ii += 1
      }
      sum
    }
    private val strLen = (x :String) => x.length
    private val constZero = (_ :Any) => 0

### Collector

    def collectSumLen[A] (data :Array[A], pf :Collector[A,String]): Int = {
      var sum = 0
      var ii = 0
      while (ii < data.length) {
        try {
          sum += pf(data(ii)).length
        } catch {
          case ex: Collector.UndefinedError => // nada
        }
        ii += 1
      }
      sum
    }

### LT0 `{ case x if (java.lang.Integer.parseInt(str) < 0) => x }`

    [info]       PartialFunction   PartFuncPlus       SemiFunc      Collector
    [info]   Mostly hit   675236   447156 0.66x   456765 0.68x   648598 0.96x
    [info]        50/50   528253   423013 0.80x   435028 0.82x  1319757 2.50x
    [info]  Mostly miss   444505   442670 1.00x   451413 1.02x  2015907 4.54x
    [info]   Total (<0)  1647994  1312839 0.80x  1343206 0.82x  3984262 2.42x

### Left `{ case Left(left) => left }`

    [info]       PartialFunction   PartFuncPlus       SemiFunc      Collector
    [info]   Mostly hit   213299   178654 0.84x   181217 0.85x   396479 1.86x
    [info]        50/50   211189   196341 0.93x   198751 0.94x  1101416 5.22x
    [info]  Mostly miss   161344   166029 1.03x   169408 1.05x  1750406 10.85x
    [info] Total (Left)   585832   541024 0.92x   549376 0.94x  3248301 5.54x

### List `{ case h :: t => h }`

    [info]       PartialFunction   PartFuncPlus       SemiFunc      Collector
    [info]   Mostly hit   215342   179145 0.83x   181028 0.84x   397644 1.85x
    [info]        50/50   212814   193569 0.91x   198951 0.93x  1095859 5.15x
    [info]  Mostly miss   161425   161260 1.00x   165665 1.03x  1743893 10.80x
    [info] Total (List)   589581   533974 0.91x   545644 0.93x  3237396 5.49x

### Discussion

Here we see a few interesting changes:

Firstly, `PartFuncPlus` is now outperforming `PartialFunction` exactly as we
would expect it to, given that it does not have to doubly evaluate its guard
functions. Now that it is no longer creating a new closure for `b.+=` every time
through the inner loop, we can see that not having to doubly evaluate the
guards is a bigger performance gain than the small bit of indirection required
by passing a function into `mapOrElse`. That said, the natural usage of
`mapOrElse` is going to involve creating a closure, which means that developers
will probably not tend to write performant code.

A second interesting change is that `SemiFunc`'s use of exceptions is getting
optimized into a goto by Hotspot. As a result, it's performance is comparable
to `PartFuncPlus`, thanks to only having to evaluate the guards once. However,
even when the optimization takes place, it suffers from the same drawback that
the natural usage of `mapOrElse` or `mapIf` will involve creating a closure.

Further more, it seems very easy to exceed the threshold for inlining that
Hotspot needs to have a wide enough horizon to do the exception optimization.
As we can see, `Collector` is still not getting optimized, perhaps because the
exception catching takes place in the more complex `collectSumLen` method
rather than in `SemiFunc`'s simplier `mapOrElse`.

## Conclusions

From a performance standpoint, it's hard to make a case for any of these
alternatives. The most consistent alternative `PartFuncPlus`, encourages a
usage pattern that introduces an extra closure creation into every call, which
ends up a wash or slightly slower unless the guard execution is sufficiently
expensive. It also may be less amenable to efficient specialization when
dealing with primitive types.

The `SemiFunc` and `Collector` approaches seem too unpredictable, given their
heavy reliance on Hotspots exception optimization which can easily be
undermined.
