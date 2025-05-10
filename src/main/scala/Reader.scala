case class Reader[E, A](run: E => A):
  def apply(e: E): A = run(e)
  
  def flatMap[B](f: A => Reader[E, B]): Reader[E, B] =
    Reader(
      e =>
        f(run(e)).run(e)
    )

  def map[B](f: A => B): Reader[E, B] =
    Reader(
      e =>
        f(run(e))
    )
object Reader:
  def pure[E, A](a: A):Reader[E, A] = Reader(_ => a)

  given [E]: Monad[[A] =>> Reader[E, A]] with
    def pure[A](a: A): Reader[E, A] = Reader.pure(a)
    def flatMap[A, B](ma: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] =
      ma.flatMap(f)
