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

  implicit class MapOps[K, V](m: Map[K, V]) {
    def update(key: K, orElse: => V): (V => V) => Map[K, V] =
      f => m.updated(key, f(m.getOrElse(key, orElse)))
  }
}