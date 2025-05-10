class Parser(input: List[Char]) 
{ 
  def parse(): List[Command] = {
    parseCommands(input, Nil).reverse
  }

  private def parseCommands(
    input: List[Char],
    acc: List[Command]
  ): List[Command] = input match {
    case Nil => acc
    case _ =>
      println(acc.headOption)
      parseIMP(input) match {
        case (remainingInput: List[Char], Some(cmd)) => 
          parseCommands(remainingInput.tail, cmd :: acc)
        case (remainingInput: List[Char], None) =>
          parseCommands(remainingInput, acc)
        case _ => 
          throw Exception("Gugu Gaga")
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
    case '\t' :: rest => 
      (rest, None)
    case '\n' :: rest => 
      parseFlowCommand(rest)
    case _ => 
      (input.tail, None)
  }

  private def parseInt(input: List[Char]): (Option[Int], Int) = 
    input
    .takeWhile(element => element != '\n')
    .map { character => character match
      case ' '  => (Some(0), 1)
      case '\t' => (Some(1), 1)
      case _ => (None, 1)
    }
    .fold((None, 0)) { (accumulator, element) => element match {
        case (Some(digit), _) => 
          accumulator match {
            case (None, length) => (Some(digit), length + 1)
            case (Some(value), length) => (Some((value << 1) + digit), length + 1)
          }
        case (None, _) => (accumulator._1, accumulator._2 + 1)
      }
    }
    
  private def parseLabel(
    input: List[Char]
  ): Option[String] = 
    input
    .takeWhile(element => element != '\n') match {
      case Nil => None
      case list => Some(list.toString())
    }

  private def parseStackCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' ' :: rest =>
      parseInt(rest) match {
        case (Some(n), length) => (rest.drop(length), Some(Command.Push(n)))
        case (None, _) => (rest, None)
      }
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  =>
          parseInt(rest.tail) match {
            case (Some(n), length) => (rest.tail.drop(length), Some(Command.Copy(n)))
            case (None, _) => (rest.tail, None)
          }
        case '\n' =>
          parseInt(rest.tail) match {
            case (Some(n), length) => (rest.tail.drop(length), Some(Command.Slide(n)))
            case (None, _) => (rest.tail, None)
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
        case _ => (rest.tail, None)
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
        case _ => (rest.tail, None)
      }
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.Div))
        case '\t' => (rest.tail, Some(Command.Mod))
        case _ => (rest.tail, None)
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
            case None => (rest.tail, None)
          }
        case '\t' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Call(label)))
            case None => (rest.tail, None)
          }
        case '\n' if rest.tail.nonEmpty =>
          parseLabel(rest.tail) match {
            case Some(label) => (rest.tail.drop(label.length + 1), Some(Command.Jump(label)))
            case None => (rest.tail, None)
          }
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
        }
    case '\n' :: '\n' :: '\n' :: rest => (rest, Some(Command.End))
    case _ => (input, None)
}

  private def parseIOCommand(
    input: List[Char]
  ): (List[Char], Option[Command]) = input match {
    case ' ' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.OutChar))
        case '\t' => (rest.tail, Some(Command.OutNum))
        case _ => (rest, None)
      }
    case '\t' :: rest if rest.nonEmpty =>
      rest.head match {
        case ' '  => (rest.tail, Some(Command.InChar))
        case '\t' => (rest.tail, Some(Command.InNum))
        case _ => (rest, None)
      }
    case _ => (input, None)
  }

}