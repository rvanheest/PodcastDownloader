package org.github.rvanheest.podcast.auto

import better.files.File
import cats.syntax.option._

import scala.io.StdIn
import scala.util.Try

object FileIO {

  def workingDirectory(workingDir: Option[File]): Try[File] = {
    workingDir map validateWorkingDirectory getOrElse askWorkingDirectory()
  }

  private def askWorkingDirectory(): Try[File] = {
    val dirString = StdIn.readLine("In which directory is podcasts.xml located? - ").trim
    validateWorkingDirectory(File(dirString)) orElse askWorkingDirectory()
  }

  private def validateWorkingDirectory(dir: File): Try[File] = Try {
    if (dir.notExists)
      throw IllegalArgumentException(s"download destination '$dir' does not exist")

    if (!dir.isDirectory)
      throw IllegalArgumentException(s"download destination '$dir' is not a directory")

    dir
  }

  def podcastsXmlLocation(dir: File): Try[File] = validatePodcastsXmlLocation(dir / "podcasts.xml")

  private def validatePodcastsXmlLocation(file: File): Try[File] = Try {
    if (file.notExists)
      throw IllegalArgumentException(s"download destination '$file' does not exist")

    if (!file.isRegularFile)
      throw IllegalArgumentException(s"download destination '$file' is not a file")

    file
  }
}
