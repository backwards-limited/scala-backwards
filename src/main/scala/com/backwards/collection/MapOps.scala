package com.backwards.collection

import java.util.Properties

object MapOps extends MapOps

trait MapOps {
  implicit def map2Properties[K, V]: Map[K, V] => Properties =
    m => (new Properties /: m) {
      case (properties, (k, v)) =>
        properties.put(k.asInstanceOf[AnyRef], v.asInstanceOf[AnyRef])
        properties
    }
}