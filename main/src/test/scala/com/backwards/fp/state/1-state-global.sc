// State management - global
var calls: Int = 0

val inc: Int => Int =
  i => {
    calls = calls + 1
    i + 1
  }

val times2: Int => Int =
  i => {
    calls = calls + 1
    i * 2
  }

// times2(inc(10))
(inc andThen times2)(10)

calls