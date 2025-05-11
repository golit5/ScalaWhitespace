object Main extends App {
  val config = FileConfig("C:/Users/Golit/Documents/git/whitespace/src/main/scala/data copy.whitespace")
  
  val linedSource = FileProcessor.processFile.run(config)

  val source = linedSource.reduceLeft( (accumulator: String, element) => 
    accumulator + '\n' + element
  ).appended('\n')
  .toList
  
  print(Interpreter.runProgram(source)._2)
}