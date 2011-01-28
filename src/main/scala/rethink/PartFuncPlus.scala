package rethink

trait PartFuncPlus[-A, +B] extends (A => B) {

  def isDefinedAt(x: A): Boolean

  def apply(x :A) = applyOrElse(x, (_ :A) => throw new MatchError)

  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
    mapOrElse(x, identity[B1], default)

  def mapOrElse[A1 <: A, B1 >: B, C](x :A1, f: B1 => C, default: A1 => C): C

  def mapIf[A1 <: A, B1 >: B](x :A1, f: B1 => Unit): Unit = mapOrElse(x, f, PartFuncPlus.constUnit)

  def orElse[A1 <: A, B1 >: B](that: PartFuncPlus[A1, B1]) : PartFuncPlus[A1, B1] =
    new PartFuncPlus[A1, B1] {
    def isDefinedAt(x: A1): Boolean =
      PartFuncPlus.this.isDefinedAt(x) || that.isDefinedAt(x)
    def mapOrElse[A2 <: A1, B2 >: B1, C](x :A2, f: B2 => C, default: A2 => C): C =
      PartFuncPlus.this.mapOrElse(x, f, (v :A2) => that.mapOrElse(v, f, default))
  }

  override def andThen[C](k: B => C) : PartFuncPlus[A, C] = new PartFuncPlus[A, C] {
    def isDefinedAt(x: A): Boolean = PartFuncPlus.this.isDefinedAt(x)
    def mapOrElse[A1 <: A, C1 >: C, D](x :A1, f: C1 => D, default: A1 => D): D =
      PartFuncPlus.this.mapOrElse(x, f compose k, default)
  }

  def lift: A => Option[B] = { x => mapOrElse(x, (v :B) => Some(v), PartFuncPlus.constNone) }
}

object PartFuncPlus
{
  def cond[T](x: T)(pf: PartFuncPlus[T, Boolean]): Boolean =
    pf.mapOrElse(x, identity[Boolean], constFalse)

  def condOpt[T,U](x: T)(pf: PartFuncPlus[T, U]): Option[U] =
    pf.mapOrElse(x, (v :U) => Some(v), constNone)

  private val constFalse = (a: Any) => false
  private val constNone = (a: Any) => None
  private val constUnit = (a: Any) => ()
}
