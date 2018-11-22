package com.backwards.fp

sealed abstract class Maybe[+A]

final case class Just[A](value: A) extends Maybe[A]

final case class Nothing[A]() extends Maybe[A]