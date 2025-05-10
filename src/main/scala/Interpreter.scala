import State.pure
import scala.util.{Try, Success, Failure}
type Interpreter[A] = State[InterpreterState, A]

object Interpreter {
  
  private def getState: Interpreter[InterpreterState] = State ( state => 
    State.pure[InterpreterState, InterpreterState](state).run(state)
  )

  private def push(value: Int): Interpreter[Either[String, Unit]] = State ( state =>
      state.push(value) match {
        case Right(newState)  => (newState, Right(()))
        case Left(error) => (state, Left(error + f"\nwith state.push($value) in Interpreter.push($value)"))
      }
    )

  private def pop: Interpreter[Either[String, Int]] = State ( state =>
    state.pop match {
      case Right(newState, value) => (newState, Right(value))
      case Left(error) => (state, Left(error + "\nwith state.pop in Interpreter.pop"))
    }
  )

  private def peek: Interpreter[Either[String, Int]] = State ( state =>
    state.peek match {
      case Right(value) => (state, Right(value))
      case Left(error) => (state, Left(error + "\nwith state.peek in Interpreter.peek"))
    }
  )

  private def peek(n: Int): Interpreter[Either[String, Int]] = State ( state =>
    state.peek(n) match {
      case Right(value) => (state, Right(value))
      case Left(error) => (state, Left(error + f"\nwith state.peek($n) in Interpreter.peek($n)"))
    }
  )

  private def dropStackWithTop(n: Int): Interpreter[Either[String, Unit]] = State ( state =>
    state.dropStack(n) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.dropStack($n) in Interpreter.dropStack($n)"))
    }
  )

  private def dropStack(n: Int): Interpreter[Either[String, Unit]] = for {
    top <- pop
    _   <- dropStackWithTop(n)
    _   <- push(top.right.get)
  } yield Right(())
  
  private def updateHeap(address: Int, value: Int): Interpreter[Either[String, Unit]] = State ( state =>
    state.updateHeap(address, value) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.updateHeap($address, $value) in Interpreter.updateHeap($address, $value)"))
    }
  )

  private def getFromHeap(address: Int): Interpreter[Either[String, Int]] = State ( state =>
    state.getFromHeap(address) 
    match {
      case Right(value) => (state, Right(value))
      case Left(error) => (state, Left(error + f"\nwith state.getFromHeap($address) in Interpreter.getFromHeap($address)"))
    }
  )

  private def duplicate: Interpreter[Either[String, Unit]] = for {
    value <- peek
    _     <- push(value.right.get)
  } yield Right(())

  private def duplicate(n: Int): Interpreter[Either[String, Unit]] = for {
      value <- peek(n)
      _     <- push(value.right.get)
    } yield Right(())

  private def swap: Interpreter[Either[String, Unit]] = for {
    b <- pop
    a <- pop
    _ <- push(b.right.get)
    _ <- push(b.right.get)
  } yield Right(())

  private def discard: Interpreter[Either[String, Unit]] = for {
    _ <- pop
  } yield Right(())

  private def slide(n: Int): Interpreter[Either[String, Unit]] = for {
    top <- pop
    _   <- dropStack(n)
    _   <- push(top.right.get)
  } yield Right(())
  
  private def add: Interpreter[Either[String, Unit]] = for {
    b <- pop
    a <- pop
    _ <- push(a.right.get + b.right.get)
  } yield Right(())

  private def sub: Interpreter[Either[String, Unit]] = for {
    b <- pop
    a <- pop
    _ <- push(a.right.get - b.right.get)
  } yield Right(())

  private def mul: Interpreter[Either[String, Unit]] = for {
    b <- pop
    a <- pop
    _ <- push(a.right.get * b.right.get)
  } yield Right(())

  private def div: Interpreter[Either[String, Unit]] = for {
    b <- pop
    a <- pop
    _ <- push(a.right.get / b.right.get)
  } yield Right(())

  private def mod: Interpreter[Either[String, Unit]] = for {
    b <- pop
    a <- pop
    _ <- push(a.right.get % b.right.get)
  } yield Right(())

  private def store: Interpreter[Either[String, Unit]] = for {
    address <- pop
    value   <- pop
    _ <- updateHeap(address.right.get, value.right.get)
  } yield Right(())

  private def retrieve: Interpreter[Either[String, Unit]] = for {
    address <- pop
    value   <- getFromHeap(address.right.get)
    _       <- push(value.right.get)
  } yield Right(())

  private def jump(label: String): Interpreter[Either[String, Unit]] = State ( state =>
    state.jumpTo(label) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.jumpTo($label) in Interpreter.jump($label)"))
    }
  )

  private def jz(label: String): Interpreter[Either[String, Unit]] = 
    peek.flatMap {
      case Right(0) => jump(label)
      case Right(_) => State.pure(Right(())) 
      case Left(error) => State.pure(Left(error + f"\nwith state.peek in Interpreter.jz($label)")) 
    }

  private def jn(label: String): Interpreter[Either[String, Unit]] = 
    peek.flatMap {
      case Right(value) if value < 0 => jump(label)
      case Right(_) => State.pure(Right(()))
      case Left(error) => State.pure(Left(error + f"\nwith state.peek in Interpreter.jn($label)")) 
    }

  private def getPosition: Interpreter[Int] = State ( state => (state, state.getPosition) )

  private def mark(label: String): Interpreter[Either[String, Unit]] = State ( state => 
    state.addLabel(label, state.getPosition) match {
      case Right(newState) => (newState, Right(()))
      case Left(error) => (state, Left(error + f"\nwith state.addLabel($label, ${state.getPosition})) in Interpreter.mark($label)"))
    }
  )

  private def call(label: String): Interpreter[Either[String, Unit]] = for {
    position <- getPosition
    _        <- push(position)
    _        <- jump(label)
  } yield Right(())

  private def ret: Interpreter[Either[String, Unit]] = ???

  private def progEnd: Interpreter[Either[String, Unit]] = ???

  private def outChar: Interpreter[Either[String, Unit]] = State { state =>
    state.pop match {
      case Right(newState, integer) => (newState.addOutput(integer.toChar.toString()), Right(()))
      case Left(error)   => (state, Left(error + f"\nwith state.pop in Interpreter.outChar"))
    }
  }

  private def outNum: Interpreter[Either[String, Unit]] = State { state =>
    state.pop match {
      case Right(newState, integer) => (newState.addOutput(integer.toString()), Right(()))
      case Left(error)   => (state, Left(error + f"\nwith state.pop in Interpreter.outChar"))
    }
  }

  private def inChar: Interpreter[Either[String, Unit]] = ???

  private def inNum: Interpreter[Either[String, Unit]] = ???
  
  def interpret(program: List[Char]): Interpreter[Either[String, Unit]] = {
    val commands = Parser(program.toList).parse()
    commands.map( command =>
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
    .fold(State.pure[InterpreterState, Either[String, Unit]](Right(())))( (stateAccum, stateObject) => 
      State( state => 
        val (s1, v1) = stateAccum.run(state) 
        v1 match {
          case Right(value) => stateObject.run(s1) 
          case Left(error)  => (s1, Left(error))
        }
      )
    )
  }
 
  
}
