package rethink

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

  def orElse[A1 <: A, B1 >: B](that: SemiFunc[A1, B1]) : SemiFunc[A1, B1] =
    new SemiFunc[A1, B1] {
      def apply(x: A1) = SemiFunc.this.apply(x)
      override def missingCase(x: A1) = that.apply(x)
    }

  def lift: A => Option[B] = new SemiFunc[A,Option[B]] {
    def apply(x :A) = try {
      Some(SemiFunc.this.apply(x))
    } catch {
      case ex: SemiFunc.UndefinedError => None
    }
  }
}

object SemiFunc
{
  final class UndefinedError extends Error

  def cond[T](x: T)(pf: SemiFunc[T, Boolean]): Boolean = try {
    pf(x)
  } catch {
    case ex: UndefinedError => false
  }

  def condOpt[T,U](x: T)(pf: SemiFunc[T, U]): Option[U] = try {
    Some(pf(x))
  } catch {
    case ex: UndefinedError => None
  }

  private val undefined = new UndefinedError
  private val constUnit = (a: Any) => ()
}
