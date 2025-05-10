object Main extends App {
  val config = FileConfig("C:/FP/whitespace/src/main/scala/data.txt")
  
  val linedSource = FileProcessor.processFile.run(config)
  
  //result.foreach(println)

  val source = linedSource.reduceLeft( (accumulator: String, element) => 
    accumulator + '\n' + element
  )
  .toList
  
  print(Interpreter.interpret(source).run(InterpreterState.initial)._1.getOutput)
}