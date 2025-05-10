import scala.io.Source
import scala.util.{Try, Using}

case class FileConfig(path: String, chunkSize: Int = 1024)

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
      processed = 
        lines.map(str => 
          str.filter(character => character match {
          case '\t' => true
          case '\n' => true
          case ' '  => true
          case _    => false
        })
      )
    } yield processed
}