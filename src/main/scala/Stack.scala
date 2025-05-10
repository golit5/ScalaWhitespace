case class Stack[A](elements: List[A] = Nil) {
  def push(x: A): Stack[A] = Stack(x :: elements)
  def pop: (Option[A], Stack[A]) = elements match {
    case h :: t => (Some(h), Stack(t))
    case Nil => (None, this)
  }
  def peek: Option[A] = elements.headOption
  def isEmpty: Boolean = elements.isEmpty
}