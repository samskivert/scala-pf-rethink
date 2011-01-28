package rethink

object CollectorTest extends FuncImpl
{
  type PF[A,B] = Collector[A,B]

  val lessZero = new Collector[String,String] {
    def apply(x: String) = x match {
      case x if (java.lang.Integer.parseInt(x) < 0) => x
      case _ => missingCase(x)
    }
  }

  val extractLeft = new Collector[Either[String,String],String] {
    def apply(x: Either[String,String]) = x match {
      case Left(left) => left
      case _ => missingCase(x)
    }
  }

  val extractHead = new Collector[List[String],String] {
    def apply(x: List[String]) = x match {
      case h :: t => h
      case _ => missingCase(x)
    }
  }

  def collect[A,B] (data :Traversable[A], pf :Collector[A,B]): Traversable[B] = {
    val b = collection.mutable.Buffer[B]()
    for (x <- data) {
      try { b += pf(x) }
      catch { case ex: Collector.UndefinedError => /* nada */ }
    }
    b
  }

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

  private val strLen = (x :String) => x.length
  private val constZero = (_ :Any) => 0
}
