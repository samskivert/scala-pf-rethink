package rethink

object PartialFunctionTest extends FuncImpl
{
  type PF[A,B] = PartialFunction[A,B]

  val lessZero: PartialFunction[String,String] = {
    case x if (java.lang.Integer.parseInt(x) < 0) => x
  }

  val extractLeft: PartialFunction[Either[String,String],String] = {
    case Left(left) => left
  }

  val extractHead: PartialFunction[List[String],String] = {
    case h :: t => h
  }

  def collect[A,B] (data :Traversable[A], pf :PartialFunction[A,B]): Traversable[B] = {
    val b = collection.mutable.Buffer[B]()
    for (x <- data) if (pf.isDefinedAt(x)) b += pf(x)
    b
  }

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
}
