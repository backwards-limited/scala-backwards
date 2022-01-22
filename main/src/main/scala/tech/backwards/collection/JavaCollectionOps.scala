package tech.backwards.collection

import java.util.{Collection => JCollection, Iterator => JIterator}
import scala.jdk.CollectionConverters._

object JavaCollectionOps extends JavaCollectionOps

trait JavaCollectionOps {
  implicit def toJava[A](as: Seq[A]): JCollection[A] = as.asJava

  implicit def toScala[A](as: JIterator[A]): Iterator[A] = as.asScala
}