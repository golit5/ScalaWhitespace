case class State[S, A](run: S => (S, A)):
  
  def getState(s: S): S = run(s)._1
  def getValue(s: S): A = run(s)._2

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (s1, a) = run(s)
      f(a).run(s1)
    })
  
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (s1, a) = run(s)
      (s1, f(a))
    })

  def withFilter(p: A => Boolean): State[S, Option[A]] =
    State ( state =>
      val (s1, a) = run(state)
      if (p(a)) (s1, Some(a))
      else (s1, None)
    )
  

object State:
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))

  given [S]: Monad[[A] =>> State[S, A]] with
    def pure[A](a: A): State[S, A] = State.pure(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma.flatMap(f) 
