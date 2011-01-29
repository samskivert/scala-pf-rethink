package rethink

import collection.mutable.Buffer

trait FuncImpl {
  type PF[_,_]

  def name = {
    val n = getClass.getName
    n.substring(n.lastIndexOf(".")+1, n.length-5) // strip rethink. and Test$
  }

  val lessZero: PF[String,String]
  val extractLeft: PF[Either[String,String],String]
  val extractHead: PF[List[String],String]
  def collect[A,B] (data :Traversable[A], pf :PF[A,B]): Traversable[B]
  def collectSumLen[A](data: Array[A], pf: PF[A,String]): Int
}

object TestMain
{
  val rand = new java.util.Random

  def shuffle[A](a: Array[A]) = {
    var ii = a.length-1
    while (ii > 0) {
      val idx = rand.nextInt(ii+1)
      val tmp = a(ii)
      a(ii) = a(idx)
      a(idx) = tmp
      ii -= 1
    }
    a
  }

  def mkLessZeroData(low: Int, high: Int) = shuffle(low to high map(_.toString) toArray)
  val lessZeroData = List("Mostly hit" -> mkLessZeroData(-175, 25),
                          "50/50" -> mkLessZeroData(-100, 100),
                          "Mostly miss" -> mkLessZeroData(-25, 175))

  def mkEitherData(lefts: Int, rights: Int): Array[Either[String,String]] = 
    shuffle((1 to lefts map(x => Left(x.toString))) ++
            (1 to rights map(x => Right(x.toString))) toArray)
  val eitherData = List("Mostly hit" -> mkEitherData(175, 25),
                        "50/50" -> mkEitherData(100, 100),
                        "Mostly miss" -> mkEitherData(25, 175))

  def mkListData(conses: Int, empties: Int): Array[List[String]] =
    shuffle((1 to conses map(x => x.toString :: Nil)) ++
            (1 to empties map(_ => Nil)) toArray)
  val listData = List("Mostly hit" -> mkListData(175, 25),
                      "50/50" -> mkListData(100, 100),
                      "Mostly miss" -> mkListData(25, 175))

  def testCollectSumLen(iters: Int)(impl: FuncImpl) = {
    var results = Buffer[(String,Long)]()

    def runTest[A](
      pf: impl.PF[A,String],
      pfid :String,
      data :List[(String,Array[A])],
      iters: Int
    ) = {
      var sum: Long = 0
      var d = data
      while (!d.isEmpty) {
        var elapsed: Long = 0
        var ii = 0
        while (ii < iters) {
          val start = System.nanoTime
          impl.collectSumLen(d.head._2, pf)
          elapsed += (System.nanoTime - start)
          ii += 1
        }
        sum += elapsed/1000
        results += (d.head._1 -> elapsed/1000)
        d = d.tail
      }
      results += (("Total " + pfid) -> sum)
    }

    runTest(impl.lessZero, "(<0)", lessZeroData, iters)
    runTest(impl.extractLeft, "(Left)", eitherData, iters)
    runTest(impl.extractHead, "(List)", listData, iters)

    results.toSeq
  }

  def testCollect(iters: Int)(impl: FuncImpl) = {
    var results = Buffer[(String,Long)]()

    def runTest[A](
      pf: impl.PF[A,String],
      pfid :String,
      data :List[(String,Array[A])],
      iters: Int
    ) = {
      var sum: Long = 0
      var d = data
      while (!d.isEmpty) {
        var elapsed: Long = 0
        var ii = 0
        while (ii < iters) {
          val start = System.nanoTime
          impl.collect(d.head._2, pf)
          elapsed += (System.nanoTime - start)
          ii += 1
        }
        sum += elapsed/1000
        results += (d.head._1 -> elapsed/1000)
        d = d.tail
      }
      results += (("Total " + pfid) -> sum)
    }

    runTest(impl.lessZero, "(<0)", lessZeroData, iters)
    runTest(impl.extractLeft, "(Left)", eitherData, iters)
    runTest(impl.extractHead, "(List)", listData, iters)

    results.toSeq
  }

  def main (args: Array[String]) {
    val tests = List(PartFuncPlusTest, SemiFuncTest, CollectorTest)

    for ((func, tester) <- List(("collectSumLen" -> testCollectSumLen _),
                                ("collect" -> testCollect _))) {
      println("--- Warming up (" + func + ") ---")
      (PartialFunctionTest :: tests) foreach tester(25000)

      println("--- Warming up again (" + func + ") ---")
      (PartialFunctionTest :: tests) foreach tester(25000)

      println("--- Testing (" + func + ") ---")
      val base = tester(50000)(PartialFunctionTest)
      val exps = tests map tester(50000)

      println("--- Testing complete (" + func + ") ---")

      printf("     %16s", PartialFunctionTest.name)
      tests foreach { t => printf(" %14s", t.name) }
      println("")

      for (ii <- 0 until base.size) {
        printf("%12s %8d", base(ii)._1, base(ii)._2)
        for (exp <- exps) {
          printf(" %8d %1.2fx", exp(ii)._2, (exp(ii)._2.toDouble / base(ii)._2))
        }
        println("")
      }
    }
  }
}
