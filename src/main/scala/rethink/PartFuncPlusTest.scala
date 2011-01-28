package rethink

object PartFuncPlusTest extends FuncImpl
{
  type PF[A,B] = PartFuncPlus[A,B]

  val lessZero = new PartFuncPlus[String,String] {
    def isDefinedAt(x :String) = x match {
      case x if (java.lang.Integer.parseInt(x) < 0) => true
      case _ => false
    }

    def mapOrElse[A1 <: String, B1 >: String, C](x: A1, f: B1 => C, default: A1 => C): C = x match {
      case x if (java.lang.Integer.parseInt(x) < 0) => f(x)
      case _ => default(x)
    }
  }

  val extractLeft = new PartFuncPlus[Either[String,String],String] {
    def isDefinedAt(x :Either[String,String]) = x match {
      case Left(x) => true
      case _ => false
    }

    def mapOrElse[A1 <: Either[String,String], B1 >: String, C](x: A1, f: B1 => C, default: A1 => C): C = x match {
      case Left(left) => f(left)
      case _ => default(x)
    }
  }

  val extractHead = new PartFuncPlus[List[String],String] {
    def isDefinedAt(x :List[String]) = x match {
      case h :: t => true
      case _ => false
    }

    def mapOrElse[A1 <: List[String], B1 >: String, C](x: A1, f: B1 => C, default: A1 => C): C = x match {
      case h :: t => f(h)
      case _ => default(x)
    }
  }

  def collect[A,B] (data :Traversable[A], pf :PartFuncPlus[A,B]): Traversable[B] = {
    val b = collection.mutable.Buffer[B]()
    for (x <- data) pf.mapIf(x, b.+=)
    b
  }

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
}
