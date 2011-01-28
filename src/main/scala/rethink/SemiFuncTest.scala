package rethink

object SemiFuncTest extends FuncImpl
{
  type PF[A,B] = SemiFunc[A,B]

  val lessZero = new SemiFunc[String,String] {
    def apply(x: String) = x match {
      case x if (java.lang.Integer.parseInt(x) < 0) => x
      case _ => missingCase(x)
    }
  }

  val extractLeft = new SemiFunc[Either[String,String],String] {
    def apply(x: Either[String,String]) = x match {
      case Left(left) => left
      case _ => missingCase(x)
    }
  }

  val extractHead = new SemiFunc[List[String],String] {
    def apply(x: List[String]) = x match {
      case h :: t => h
      case _ => missingCase(x)
    }
  }

  def collect[A,B] (data :Traversable[A], pf :SemiFunc[A,B]): Traversable[B] = {
    val b = collection.mutable.Buffer[B]()
    for (x <- data) pf.mapIf(x, b.+=)
    b
  }

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
}
