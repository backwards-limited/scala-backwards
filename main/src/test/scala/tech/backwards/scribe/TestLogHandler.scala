package tech.backwards.scribe

import scala.collection.mutable.ListBuffer
import scribe.LogRecord
import scribe.handler.LogHandler

class TestLogHandler extends LogHandler {
  private val records: ListBuffer[LogRecord] =
    ListBuffer.empty[LogRecord]

  override def log(record: LogRecord): Unit =
    records += record

  def clear(): Unit =
    records.clear()

  def logRecords: List[LogRecord] =
    records.toList
}