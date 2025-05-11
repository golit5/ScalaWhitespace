import scala.io.Source

case class FileConfig(path: String)

type FileIO[A] = Reader[FileConfig, A]

object FileReader {
  def readLines: FileIO[LazyList[String]] = Reader { config =>
    val source = Source.fromFile(config.path)
    val iterator = source.getLines()
    
    LazyList.unfold(iterator) { iter =>
      if (iter.hasNext) {
        Some((iter.next(), iter))
      } else {
        source.close()
        None
      }
    }
  }
}

object FileProcessor {
  def processFile: FileIO[LazyList[String]] = 
    for {
      lines <- FileReader.readLines
      processed = lines
      .map(str => 
          str.filter(character => character match {
          case '\t' => true
          case '\n' => true
          case ' '  => true
          case _    => false
        } )
      )
    } yield processed
}