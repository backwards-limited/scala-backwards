package com.backwards.fp

final case class Writer[W, A](run: () => (W, A))