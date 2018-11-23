package com.backwards.logging

import wvlet.log.LogFormatter._
import wvlet.log.LogTimestampFormatter.formatTimestamp
import wvlet.log.{LogFormatter, LogRecord, LogSupport}
import wvlet.log.Logger._

trait Logging extends LogSupport {
  def logFormatter: LogFormatter = BackwardsLogFormatter

  scheduleLogLevelScan
  setDefaultFormatter(logFormatter)
}

object BackwardsLogFormatter extends LogFormatter {
  override def formatLog(r: LogRecord): String = {
    val loc =
      r.source
        .map(source => s" ${withColor(Console.YELLOW, s"- (${source.fileLoc})")}")
        .getOrElse("")

    val logTag = highlightLog(r.level, r.level.name)

    val log =
      f"${withColor(Console.YELLOW, formatTimestamp(r.getMillis))} $logTag%14s [${withColor(Console.WHITE, r.leafLoggerName)}] ${highlightLog(r.level, r.getMessage)} $loc"

    appendStackTrace(log, r)
  }
}