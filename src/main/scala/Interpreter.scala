import State.pure
import scala.util.{Try, Success, Failure}

case class Interpreter[A](run: InterpreterState => (InterpreterState, Either[String, A])):

  def apply(s: InterpreterState): (InterpreterState, Either[String, A]) = 
    run(s)

  def map[B](f: A => B): Interpreter[B] =
    Interpreter { s =>
      val (s1, res) = run(s)
      res match {
        case Left(err) => (s1, Left(err))
        case Right(a)  => (s1, Right(f(a)))
      }
    }

  def flatMap[B](f: A => Interpreter[B]): Interpreter[B] =
    Interpreter { s =>
      val (s1, res) = run(s)
      res match {
        case Left(err) => (s1, Left(err))
        case Right(a)  => f(a).run(s1)
      }
    }

  def withFilter(p: A => Boolean): Interpreter[Option[A]] =
    Interpreter { s =>
      val (s1, res) = run(s)
      res match {
        case Left(err) => (s1, Left(err))
        case Right(a)  => 
          if (p(a)) (s1, Right(Some(a)))
          else (s1, Right(None)) 
      }
    }


object Interpreter:
  def pure[A](a: Either[String, A]): Interpreter[A] = Interpreter(s => (s, a))
  def success[A](a: A): Interpreter[A] = pure(Right(a))
  def fail(error: String): Interpreter[Nothing] = pure(Left(error))
  
  private def getState: Interpreter[InterpreterState] = Interpreter ( state => 
    State.pure(Right(state)).run(state)
  )

  private def push(value: Int): Interpreter[Unit] = Interpreter ( state =>
      state.push(value) match {
        case Right(newState)  => (newState, Right(()))
        case Left(error) => (state, Left(error + f"\nwith state.push($value) in Interpreter.push($value)"))
      }
    )

  private def pop: Interpreter[Int] = Interpreter ( state =>
    state.pop match {
      case Right(newState, value) => (newState, Right(value))
      case Left(error) => (state,  Left(error + "\nwith state.pop in Interpreter.pop"))
    }
  )

  private def peek: Interpreter[Int] = Interpreter ( state =>
    state.peek match {
      case Right(value) => (state, Right(value))
      case Left(error) => (state, Left(error + "\nwith state.peek in Interpreter.peek"))
    }
  )

  private def peek(n: Int): Interpreter[Int] = Interpreter ( state =>
    state.peek(n) match {
      case Right(value) => (state, Right(value))
      case Left(error) => (state, Left(error + f"\nwith state.peek($n) in Interpreter.peek($n)"))
    }
  )

  private def dropStackWithTop(n: Int): Interpreter[Unit] = Interpreter ( state =>
    state.dropStack(n) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.dropStack($n) in Interpreter.dropStack($n)"))
    }
  )


  private def dropStack(n: Int): Interpreter[Unit] = for {
    top <- pop
    _   <- dropStackWithTop(n)
    _   <- push(top)
  } yield Right(())
  
  private def updateHeap(address: Int, value: Int): Interpreter[Unit] = Interpreter ( state =>
    state.updateHeap(address, value) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.updateHeap($address, $value) in Interpreter.updateHeap($address, $value)"))
    }
  )

  private def getFromHeap(address: Int): Interpreter[Int] = Interpreter ( state =>
    state.getFromHeap(address) 
    match {
      case Right(value) => (state, Right(value))
      case Left(error) => (state, Left(error + f"\nwith state.getFromHeap($address) in Interpreter.getFromHeap($address)"))
    }
  )

  private def duplicate: Interpreter[Unit] = for {
    value <- peek
    _     <- push(value)
  } yield Right(())

  private def duplicate(n: Int): Interpreter[Unit] = for {
      value <- peek(n)
      _     <- push(value)
  } yield Right(())

  private def swap: Interpreter[Unit] = for {
    b <- pop
    a <- pop
    _ <- push(b)
    _ <- push(b)
  } yield Right(())

  private def discard: Interpreter[Unit] = for {
    _ <- pop
  } yield Right(())

  private def slide(n: Int): Interpreter[Unit] = for {
    top <- pop
    _   <- dropStack(n)
    _   <- push(top)
  } yield Right(())
  
  private def add: Interpreter[Unit] = for {
    b <- pop
    a <- pop
    _ <- push(a + b)
  } yield Right(())

  private def sub: Interpreter[Unit] = for {
    b <- pop
    a <- pop
    _ <- push(a - b)
  } yield Right(())

  private def mul: Interpreter[Unit] = for {
    b <- pop
    a <- pop
    _ <- push(a * b)
  } yield Right(())

  private def div: Interpreter[Unit] = for {
    b <- pop
    a <- pop
    _ <- push(a / b)
  } yield Right(())

  private def mod: Interpreter[Unit] = for {
    b <- pop
    a <- pop
    _ <- push(a % b)
  } yield Right(())

  private def store: Interpreter[Unit] = for {
    address <- pop
    value   <- pop
    _ <- updateHeap(address, value)
  } yield Right(())

  private def retrieve: Interpreter[Unit] = for {
    address <- pop
    value   <- getFromHeap(address)
    _       <- push(value)
  } yield Right(())

  private def jump(label: String): Interpreter[Unit] = Interpreter ( state =>
    state.jumpTo(label) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.jumpTo($label) in Interpreter.jump($label)"))
    }
  )

  private def jz(label: String): Interpreter[Unit] = for {
    value <- peek
    if value == 0
    _ <- jump(label)
  } yield Right(())

  private def jn(label: String): Interpreter[Unit] = for {
    value <- peek
    if value < 0
    _ <- jump(label)
  } yield Right(())

  private def getPosition: Interpreter[Int] = Interpreter ( state => (state, Right(state.getPosition)) )

  private def mark(label: String): Interpreter[Unit] = Interpreter ( state => 
    state.addLabel(label, state.getPosition) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.addLabel($label, ${state.getPosition})) in Interpreter.mark($label)"))
    }
  )

  private def call(label: String): Interpreter[Unit] = for {
    position <- getPosition
    _        <- push(position)
    _        <- jump(label)
  } yield Right(())

  private def ret: Interpreter[Unit] = ???

  private def progEnd: Interpreter[Unit] = Interpreter { state =>
    (state, Left(state.getOutput + "\nProgram has ended!"))
  }

  private def outChar: Interpreter[Unit] = Interpreter { state =>
    state.pop match {
      case Right(newState, integer) => (newState.addOutput(integer.toChar.toString()), Right(()))
      case Left(error)   => (state, Left(error + f"\nwith state.pop in Interpreter.outChar"))
    }
  }

  private def outNum: Interpreter[Unit] = Interpreter { state =>
    state.pop match {
      case Right(newState, integer) => (newState.addOutput(integer.toString()), Right(()))
      case Left(error)   => (state, Left(error + f"\nwith state.pop in Interpreter.outChar"))
    }
  }

  private def inChar: Interpreter[Unit] = ???

  private def inNum: Interpreter[Unit] = ???
  
  def interpret(program: List[Char]): List[Interpreter[Unit]] = {
    Parser(program)
    .parse()
    .map( command =>
      command match{
        case Command.Push(value)  => push(value)
        case Command.Dup          => duplicate
        case Command.Copy(n)      => duplicate(n)
        case Command.Swap         => swap
        case Command.Discard      => discard
        case Command.Slide(n)     => slide(n)
        case Command.Add          => add
        case Command.Sub          => sub
        case Command.Mul          => mul
        case Command.Div          => div
        case Command.Mod          => mod
        case Command.Store        => store
        case Command.Retrieve     => retrieve
        case Command.Mark(label)  => mark(label)
        case Command.Call(label)  => call(label)
        case Command.Jump(label)  => jump(label)
        case Command.Jz(label)    => jz(label)
        case Command.Jn(label)    => jn(label)
        case Command.Ret          => ret
        case Command.End          => progEnd
        case Command.OutChar      => outChar
        case Command.OutNum       => outNum
        case Command.InChar       => inChar
        case Command.InNum        => inNum
      }
    )
  }

  def runProgram(
    program: List[Char],
    initialState: InterpreterState = InterpreterState.initial
  ): (InterpreterState, String) = {
    val commands = interpret(program)
    @annotation.tailrec
    def loop(
      state: InterpreterState,
      stepsLeft: Int = 1000
    ): (InterpreterState, String) = {
      if (stepsLeft <= 0) (state, state.getOutput + "\nMaximum execution steps exceeded")
      else commands(state.getPosition).run(state) match {
        case (newState, Left(message)) => 
          (newState, message)
        case (newState, Right(_)) =>
          loop(newState.advanceCounter(1), stepsLeft - 1)
      }
    }
    loop(initialState)
  }

