package rethink

trait Collector[-A, +B] extends (A => B)
{
  def missingCase(x: A): B = throw Collector.undefined
}

object Collector
{
  final class UndefinedError extends Error

  private val undefined = new UndefinedError
  private val constUnit = (a: Any) => ()
}
