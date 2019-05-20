package com.backwards.collection

import java.util.{Collection => JCollection, Iterator => JIterator}
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object JavaCollectionOps extends JavaCollectionOps

trait JavaCollectionOps {
  implicit def toJava[A](as: Seq[A]): JCollection[A] = asJavaCollection(as)

  implicit def toScala[A](as: JIterator[A]): Iterator[A] = as.asScala
}