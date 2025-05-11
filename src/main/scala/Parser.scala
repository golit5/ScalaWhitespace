class Parser(input: List[Char]) 
{ 
  def parse(): List[Command] = {
    val result = parseCommands(input, Nil).reverse
    println(result)
    result
  }

  private def parseCommands(
    input: List[Char],
    acc: List[Command]
  ): List[Command] = input match {
    case Nil => acc
    case _ =>
      parseIMP(input) match {
        case (remainingInput, Some(cmd)) => 
            parseCommands(remainingInput, cmd :: acc)
        case (remainingInput, None) =>
          parseCommands(remainingInput, acc)
      }
  }

  private def parseIMP(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case Nil => (input, None)
    case ' ' :: rest => 
      parseStackCommand(rest)
    case '\t' :: rest if rest.nonEmpty => 
      rest.head match {
        case ' ' => 
          parseArithmeticCommand(rest.tail)
        case '\t' => 
          parseHeapCommand(rest.tail)
        case '\n' => 
          parseIOCommand(rest.tail)
        case _ => 
          (rest, None)
      }
    case '\n' :: rest if rest.nonEmpty => 
      parseFlowCommand(rest)
    case _ :: rest => 
      (rest, None)
  }

  private def parseInt(
    input: List[Char]
  ): (Option[Int], Int) = 
    val digitList = input
    .takeWhile(element => element != '\n')
    .map { character => character match
      case ' '  => Some(0)
      case '\t' => Some(1)
      case _ => None
    }
    (
      digitList
      .fold(None) { (accumulator, element) => element match {
          case Some(digit) => 
            accumulator match {
              case None => Some(digit)
              case Some(value) => Some((value << 1) + digit)
            }
          case None => accumulator
      } },
      digitList.length
    )
    
  private def parseLabel(
    input: List[Char]
  ): Option[String] = 
    input
    .takeWhile(_ != '\n')
    .mkString match {
      case "" => None
      case s => Some(s)
    }

  private def parseStackCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' ' :: rest =>
      parseInt(rest) match {
        case (Some(n), length) => (rest.drop(length + 1), Some(Command.Push(n)))
        case (None, _) => (input, None)
      }
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  =>
          parseInt(rest.tail) match {
            case (Some(n), length) => (rest.tail.drop(length + 1), Some(Command.Copy(n)))
            case (None, _) => (input, None)
          }
        case '\n' =>
          parseInt(rest.tail) match {
            case (Some(n), length) => (rest.tail.drop(length + 1), Some(Command.Slide(n)))
            case (None, _) => (input, None)
          }
        case _ => (rest.tail, None)
      }
    case '\n' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  =>
          (rest.tail, Some(Command.Dup))
        case '\t' =>
          (rest.tail, Some(Command.Swap))
        case '\n' =>
          (rest.tail, Some(Command.Discard))
        case _ => (input, None)
      }
    case _ => (input, None)
  }

  private def parseArithmeticCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' ' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.Add))
        case '\t' => (rest.tail, Some(Command.Sub))
        case '\n' => (rest.tail, Some(Command.Mul))
        case _ => (input, None)
      }
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.Div))
        case '\t' => (rest.tail, Some(Command.Mod))
        case _ => (input, None)
      }
    case _ => (input, None)
  }

  private def parseHeapCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' '  :: rest => (rest, Some(Command.Store))
    case '\t' :: rest => (rest, Some(Command.Retrieve))
    case _ => (input, None)
  }

  private def parseFlowCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' '  :: rest if rest.nonEmpty =>
      rest.head match {
        case ' ' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Mark(label)))
            case None => (input, None)
          }
        case '\t' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Call(label)))
            case None => (input, None)
          }
        case '\n' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Jump(label)))
            case None => (input, None)
          }
        case _ => (input, None)
      } 
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' ' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Jz(label)))
            case None => (rest.tail, None)
          }
        case '\t' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Jn(label)))
            case None => (rest.tail, None)
          }
        case '\n' => (rest.tail, Some(Command.Ret))
        case _ => (input, None)
        }
    case '\n' :: '\n' :: rest => (rest, Some(Command.End))
    case _ => (input, None)
}

  private def parseIOCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' ' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.OutChar))
        case '\t' => (rest.tail, Some(Command.OutNum))
        case _ => (input, None)
      }
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.InChar))
        case '\t' => (rest.tail, Some(Command.InNum))
        case _ => (input, None)
      }
    case _ => (input, None)
  }

}