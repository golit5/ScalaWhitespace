sealed trait Command
object Command {
  // Stack Manipulation
  case class Push(n: Int) extends Command
  case object Dup extends Command
  case class Copy(n: Int) extends Command
  case object Swap extends Command
  case object Discard extends Command
  case class Slide(n: Int) extends Command
  
  // Arithmetic
  case object Add extends Command
  case object Sub extends Command
  case object Mul extends Command
  case object Div extends Command
  case object Mod extends Command
  
  // Heap Access
  case object Store extends Command
  case object Retrieve extends Command
  
  // Flow Control
  case class Mark(label: String) extends Command
  case class Call(label: String) extends Command
  case class Jump(label: String) extends Command
  case class Jz(label: String) extends Command
  case class Jn(label: String) extends Command
  case object Ret extends Command
  case object End extends Command
  
  // I/O
  case object OutChar extends Command
  case object OutNum extends Command
  case object InChar extends Command
  case object InNum extends Command
}