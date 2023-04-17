package org.github.rvanheest.podcast.auto

import java.io.InputStream
import java.net.URL

extension (url: URL) {
  def toInputStream: InputStream = {
    val uc = url.openConnection()
    uc.addRequestProperty("User-Agent", "Mozilla/5.0 (Windows NT 6.3; Win64; x64)")
    uc.getInputStream
  }
}
