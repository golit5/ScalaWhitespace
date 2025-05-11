import scala.util.{Try, Failure, Success}
import scala.sys.Prop
final case class InterpreterState (
  stack: Stack[Int],
  heap: Map[Int, Int],
  programCounter: Int,
  labels: Map[String, Int],
  output: String
) {
  
  def push(value: Int): Either[String, InterpreterState] = Try {
    copy(stack = stack.push(value))
  } match {
    case Success(newState) => Right(newState)
    case Failure(exception) => Left(exception.getMessage() + "\nin InterpreterState.push")
  }
  
  def pop: Either[String, (InterpreterState, Int)] = Try {
    stack.pop
   } match {
      case Success(Some(value), newStack) => Right(copy(stack = newStack), value)
      case Success(None, _) => Left("stack.pop returned None in InterpreterState.pop")
      case Failure(exception) => Left(exception.getMessage() + "\nwith stack.pop in InterpreterState.pop")
  }
  
  def peek: Either[String, Int] = Try {
    stack.peek
  } match {
    case Success(Some(value)) => Right(value)
    case Success(None) => Left("stack.peek returned None in InterpreterState.peek")
    case Failure(exception) => Left(exception.getMessage() + "\nwith stack.peek in InterpreterState.peek")
  }

  def peek(n: Int): Either[String, Int] = Try {
    stack(n)
  } match {
    case Success(value) => Right(value)
    case Failure(exception) => Left(exception.getMessage() + f"\nwith stack($n) in InterpreterState.peek($n)")
  }

  def dropStack(n: Int): Either[String, InterpreterState] = Try {
    stack.drop(n)
  } match {
    case Success(newStack) => Right(copy(stack = newStack))
    case Failure(exception) => Left(exception.getMessage() + f"\nwith Stack(stack._1.drop($n)) in InterpreterState.dropStack($n)")
  }

  def updateHeap(address: Int, value: Int): Either[String, InterpreterState] = Try {
    heap.updated(address, value)
  } match {
    case Success(newHeap) => Right(copy(heap = newHeap))
    case Failure(exception) => Left(exception.getMessage() + f"\nwith heap.updated($address, $value) in InterpreterState.updateHeap($address, $value)")
  }
  
  def getFromHeap(address: Int): Either[String, Int] = Try {
    heap.get(address)
  } match {
      case Success(Some(value)) => Right(value)
      case Success(None) => Left(f"heap.get($address) returned None in InterpreterState.getFromHeap($address)")
      case Failure(exception) => Left(exception.getMessage() + f"\nwith heap.get($address) in InterpreterState.getFromHeap($address)")
  }
  
  def advanceCounter(by: Int = 1): InterpreterState =
    copy(programCounter = programCounter + by)
  
  def jumpTo(newPosition: Int): InterpreterState =
    copy(programCounter = newPosition)

  def jumpTo(newPositionLabel: String): Either[String, InterpreterState] = Try {
    labels.get(newPositionLabel)
  } match {
    case Success(Some(value)) => Right(jumpTo(value))
    case Success(None) => Left(f"labels.get($newPositionLabel) returned None in InterpreterState.jumpTo($newPositionLabel)")
    case Failure(exception) => Left(exception.getMessage() + f"\nwith labels.get($newPositionLabel) in InterpreterState.jumpTo($newPositionLabel)")
  }
  
  def hasLabel(label: String): Boolean =
    labels.get(label).isDefined

  def addLabel(name: String, position: Int): Either[String, InterpreterState] = Try {
    labels.updated(name, position)
  } match {
    case Success(newMap) => Right(copy(labels = newMap))
    case Failure(exception) => Left(exception.getMessage() + f"\nwith labels.updated($name, $position) in InterpreterState.addLabel($name, $position)")
  }
  
  def getPosition: Int = programCounter

  def addOutput(text: String): InterpreterState =
    copy(output = output + text)

  def getOutput: String = output

  def flushOutput: InterpreterState = copy(output = "")
}

object InterpreterState {
  val initial: InterpreterState = InterpreterState(
    stack = Stack(),
    heap = Map.empty,
    programCounter = 0,
    labels = Map.empty,
    output = ""
  )
}