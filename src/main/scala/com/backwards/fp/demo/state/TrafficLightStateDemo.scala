package com.backwards.fp.demo.state

import cats.data.State
import cats.data.State._
import com.backwards.fp.demo.state.Signal._

/**
 * https://timperrett.com/2013/11/25/understanding-state-monad/
 */
object TrafficLightStateDemo extends App {
  // Let's start by assuming that the signal is entirely disabled, so we need to make it operational:
  def enable: State[Signal, Boolean] =
    for {
      a: Signal <- get
      _ <- modify((s: Signal) => s.copy(isOperational = true))
      r: Signal <- get
    } yield r.isOperational

  def change(seq: Aspect -> Mode*): State[Signal, Map[Aspect, Mode]] =
    for {
      m: Signal <- get
      _ <- modify(display(seq))
      s: Signal <- get
    } yield s.display

  def display(seq: Seq[Aspect -> Mode]): Signal => Signal = { signal =>
    if (signal.isOperational) signal.copy(display = signal.display ++ seq.toMap)
    else default
  }

  def halt: State[Signal, Map[Aspect, Mode]] =
    change(Red -> Solid, Amber -> Off, Green -> Off)

  def ready: State[Signal, Map[Aspect, Mode]] =
    change(Red -> Solid, Amber -> Solid, Green -> Off)

  def go: State[Signal, Map[Aspect, Mode]] =
    change(Red -> Off, Amber -> Off, Green -> Solid)

  def slow: State[Signal, Map[Aspect, Mode]] =
    change(Red -> Off, Amber -> Solid, Green -> Off)

  // Example
  println(halt.run(Signal(isOperational = true, Map.empty)).value)
  println()

  // Program
  val program: State[Signal, List[Signal]] /*IndexedStateT[Eval, Signal, Signal, List[Signal]]*/ = for {
    _  <- enable
    r0 <- get // Debugging
    _  <- halt
    r1 <- get // Debugging
    _  <- ready
    r2 <- get // Debugging
    _  <- go
    r3 <- get // Debugging
    _  <- slow
    r4 <- get
  } yield r0 :: r1 :: r2 :: r3 :: r4 :: Nil

  program.runA(default).value.zipWithIndex.foreach { case (signal, index) =>
    println(s"r$index - $signal")
  }
}

sealed trait Aspect

case object Green extends Aspect
case object Amber extends Aspect
case object Red   extends Aspect

sealed trait Mode

case object Off      extends Mode
case object Flashing extends Mode
case object Solid    extends Mode

/**
 * Represents the actual display set: must be enabled before it can be used.
 */
case class Signal(isOperational: Boolean, display: Map[Aspect, Mode])

object Signal {
  // A bit of sugar.
  type ->[A, B] = (A, B)

  // Convenience alias as all state ops here will deal with signal state.
  type SignalState[A] = State[Signal, A]

  // Dysfunctional lights revert to their flashing red lights to act as a stop sign to keep folks safe.
  val default: Signal = Signal(
    isOperational = false,
    display = Map(Red -> Flashing, Amber -> Off, Green -> Off)
  )
}