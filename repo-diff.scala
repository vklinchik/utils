import java.io.{File, PrintWriter}
import scala.collection.immutable.SortedSet
import scala.io.Source

object Diff {
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def scalaFilesInDirectory(path: String) = {
    SortedSet[File]() ++ recursiveListFiles(new File(path)).toSet.filter(_.getName.endsWith(".scala"))
  }

  case class RepoFile(
                       oldFile: File,
                       fileName: String,
                       newFile: Option[File],
                       changed: Option[Boolean]) {
    def diffCommand = s"ksdiff ${oldFile.getAbsolutePath} ${newFile.map(_.getAbsolutePath).getOrElse("N / A")}"

    override def toString: String = s"RepoFile(\n  oldPath = $oldFile\n  newPath = $newFile\n  changed=$changed\n)"
  }

  def changed(a: File, b: File): Boolean = {
    val startWithDropPatterns = Set("import", "package")
    val makeStartsWithFilter: (String => String => Boolean) = { (pattern: String) =>
      (line: String) => line.startsWith(pattern)
    }

    val startsWithFilters: Set[(String) => Boolean] = startWithDropPatterns.map(makeStartsWithFilter)
    val emptyFilter: String => Boolean = s => s.isEmpty
    val allFilters = startsWithFilters + emptyFilter
    val composedFilters: String => Boolean = line => allFilters.exists(f => f(line))

    val contents = (file: File) => Source.fromFile(file).getLines().dropWhile(composedFilters)

    contents(a).zip(contents(b)).exists { case (l1, l2) => l1 != l2 }
  }

  def doMatch(oldFiles: Set[File], newFiles: Set[File]) = {
    oldFiles.map { oldFile =>
      newFiles.find(newFile => newFile.getName == oldFile.getName) match {
        case Some(matchingFile) =>
          RepoFile(
            oldFile = oldFile,
            fileName = oldFile.getName,
            newFile = Some(matchingFile),
            changed = Some(changed(oldFile, matchingFile))
          )
        case None =>
          RepoFile(
            oldFile = oldFile,
            fileName = oldFile.getName,
            newFile = None,
            changed = None
          )
      }

    }
  }


  def writeToFile(fileName: String, text: String) = {
    Some(new PrintWriter(fileName)).foreach {
      p => p.write(text)
        p.close
    }
  }

  val webCmpPlayPath = "/Users/vklinchik/dev/webcmp-play"
  val webCmpPlayScalaFiles: SortedSet[File] = scalaFilesInDirectory(webCmpPlayPath)

  val cmpWebPath = "/Users/vklinchik/dev/cmp-web"
  val cmpWebScalaFiles: SortedSet[File] = scalaFilesInDirectory(cmpWebPath)

  val cmpCommonsPath = "/Users/vklinchik/dev/cmp-commons"
  val cmpCommonsScalaFiles: SortedSet[File] = scalaFilesInDirectory(cmpCommonsPath)

  val oldFilesInNewRepo: Set[RepoFile] = doMatch(webCmpPlayScalaFiles, cmpWebScalaFiles ++ cmpCommonsScalaFiles)

  val (filesWithMatches, filesWithoutMatches) = oldFilesInNewRepo.partition(_.newFile.isDefined)

  val matchedFiles = filesWithMatches.toVector.sortBy(_.oldFile.getName)
  val (changedFiles, unchangedFiles) = matchedFiles.partition(_.changed.getOrElse(false))

  val changedFilesDiffs = changedFiles.map(_.diffCommand).mkString("\n")
  val unchangedFilesDiffs = unchangedFiles.map(_.diffCommand).mkString("\n")

  val unmatchedFilesByName = filesWithoutMatches.toVector.sortBy(_.oldFile.getName)

  def run(): Unit = {
    writeToFile("changedFilesDiff.txt", changedFilesDiffs)
    writeToFile("unchangedFilesDiffs.txt", unchangedFilesDiffs)
    writeToFile("unmatchedFilesByName.txt", unmatchedFilesByName.mkString("\n"))
  }
}


Diff.run()



