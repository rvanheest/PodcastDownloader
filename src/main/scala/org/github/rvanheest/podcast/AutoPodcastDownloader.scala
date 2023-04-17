package org.github.rvanheest.podcast

import better.files.File
import org.github.rvanheest.podcast.auto.{ FileIO, Podcasts }
import cats.syntax.option._

import scala.util.{ CommandLineParser, Failure, Success }

given CommandLineParser.FromString[Option[File]] with
  override def fromString(s: String): Option[File] =
    if s.isBlank then none
    else File(s).some

@main
def autodownloader(workingDir: Option[File]): Unit = {
  val result = for {
    workingDirectory <- FileIO.workingDirectory(workingDir)
    podcastsXmlLocation <- FileIO.podcastsXmlLocation(workingDirectory)
    podcasts <- Podcasts.read(podcastsXmlLocation)
    _ <- podcasts.processPodcasts(podcastsXmlLocation)
  } yield ()

  result match {
    case Failure(e) => e.printStackTrace()
    case Success(_) => println("completed")
  }
}
