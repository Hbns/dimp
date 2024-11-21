import os.Path

// logger to write output txt/csv files
trait Logger {
  def log(line: String): Unit
}

object TestLogger extends Logger {
  private var filePath: Option[Path] = None

  def setFileName(name: String): Unit = {
    val outputDir = os.pwd / "output"
    // create folder in case it does not exist
    if (!os.exists(outputDir)) os.makeDir.all(outputDir)
    filePath = Some(outputDir / name)
  }

  // set the filePath
  def setFilePath(path: String): Unit = {
    filePath = Some(Path(path))
  }
  // get the filePath
  def getFilePath(): String = {
    filePath match {
      case Some(path) => path.toString
      case None => "No file path set"
    }
  }
  // log a line in filePath
  def log(line: String): Unit = this.synchronized{
    filePath match {
      case Some(path) => 
        try {
        os.write.append(path, line + "\n")
      } catch {
        case e: Exception =>
          println(s"Failed to write to log file: ${e.getMessage}")
          throw e
      }
      case None => throw new IllegalStateException("File path is not set. Call setFileName first.") 
    }
  }
  // delete file at current filePath
  def deleteFile(): Unit = {
    filePath.foreach(path => if (os.exists(path)) os.remove(path))
  }

  // Function to write to csv file
  def generateCsvFile(csvFilePath: os.Path, header: List[String], results: List[(Int, Int, Int)]): Unit = {
    // Ensure the output directory exists
    os.makeDir.all(csvFilePath / os.up)

    // Write header
    os.write.over(csvFilePath, header.mkString(",") + "\n")
    
    // Write rows
    results.foreach { result =>
      os.write.append(csvFilePath, result.productIterator.mkString(",") + "\n")
    }
  }
}