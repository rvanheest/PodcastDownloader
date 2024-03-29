package org.github.rvanheest.podcast.auto

import better.files.File
import cats.syntax.option._

import java.io.IOException
import java.net.URL
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.{ Success, Try }
import scala.xml.{ Node, NodeSeq, XML }

case class Podcast(title: String,
                   saveLocation: File,
                   podcastUrl: URL,
                   disabled: Boolean,
                   episodeNameTemplate: EpisodeNameTemplate,
                   lastEpisode: Option[LastEpisode],
                   skipEpisodes: Option[List[SkipEpisode]],
                  ) {

  def listEpisodesToDownload(): Try[LazyList[PodcastEpisode]] = {
    if (disabled) Success(LazyList.empty)
    else readPodcastFeed()
      .map(episodes => (lastEpisode fold episodes) (lastEpisode => episodes takeWhile (lastEpisode.identifier != _.id)))
      .map(episodes => (skipEpisodes fold episodes) (skippers => episodes filterNot (_ matchesAny skippers)))
  }

  def podcastDirectory(): File = {
    if (saveLocation.exists && saveLocation.isDirectory) saveLocation
    else saveLocation.createDirectory()
  }

  private def readPodcastFeed(): Try[LazyList[PodcastEpisode]] = {
    Try { XML.load(podcastUrl.toInputStream) } map parseRss recover {
      case e : IOException =>
        System.err.println(s"Fout bij het lezen van RSS feed $podcastUrl: ${ e.getMessage }")
        e.printStackTrace()
        LazyList.empty
    }
  }

  private def parseRss(rss: Node): LazyList[PodcastEpisode] = {
    (rss \ "channel" \ "item").to(LazyList) flatMap parseEpisode
  }

  private def parseEpisode(xml: Node): Option[PodcastEpisode] = {
    val id = (xml \ "guid").text.trim
    val title = (xml \ "title").head.text.trim
    val pubDate = parseDate((xml \ "pubDate").text.trim)
    val seasonNumber = (xml \ "season").headOption map (_.text.trim) withFilter (_.nonEmpty) map (_.toInt)
    val episodeNumber = (xml \ "episode").headOption map (_.text.trim) withFilter (_.nonEmpty) map (_.toInt)
    val subtitle = (xml \ "subtitle").headOption map (_.text.trim) filter (_.nonEmpty)

    lazy val urlFromEnclosure = xml \ "enclosure" withFilter (_ \@ "type" matches "(audio|video)/(mpeg|mp3|mp4)") map (n => URL(n \@ "url"))
    lazy val urlFromContent = xml \ "content" withFilter (_ \@ "medium" == "audio") map (n => URL(n \@ "url"))

    urlFromEnclosure.headOption orElse urlFromContent.headOption map (PodcastEpisode(id, title, pubDate, _, seasonNumber, episodeNumber, subtitle))
  }

  private def parseDate(dateString: String): String = {
    val inputFormatter = DateTimeFormatter.RFC_1123_DATE_TIME
    val outputFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    ZonedDateTime.parse(dateString, inputFormatter).toLocalDate format outputFormatter
  }

  def toXML: Node = {
    <podcast disabled={(disabled.some filter identity map (_.toString)).orNull}>
      <name>{title}</name>
      <saveLocation>{saveLocation}</saveLocation>
      <podcastUrl>{podcastUrl}</podcastUrl>
      {episodeNameTemplate.toXml}
      {lastEpisode map (_.toXML) getOrElse NodeSeq.Empty}
      {skipEpisodes map (ss => <skipEpisodes>{ss map (_.toXML)}</skipEpisodes>) getOrElse NodeSeq.Empty}
    </podcast>
  }
}
object Podcast {
  def read(xml: Node): Podcast = Podcast(
    (xml \ "name").text.trim,
    File((xml \ "saveLocation").text.trim),
    URL((xml \ "podcastUrl").text.trim),
    Option(xml \@ "disabled") filter (_.nonEmpty) exists (_.trim.toBoolean),
    EpisodeNameTemplate.read((xml \ "episodeNameTemplate").head),
    (xml \ "lastDownloadedEpisode").headOption map LastEpisode.read,
    readSkipEpisodes(xml \ "skipEpisodes" \ "skipEpisode"),
  )

  private def readSkipEpisodes(xml: NodeSeq): Option[List[SkipEpisode]] = {
    if (xml.isEmpty) none
    else xml.map(SkipEpisode.read).toList.some
  }
}
