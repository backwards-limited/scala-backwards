package com.backwards.fp

import com.backwards.fp.monoid.Monoid

final case class Writer[W: Monoid, A](run: () => (W, A))