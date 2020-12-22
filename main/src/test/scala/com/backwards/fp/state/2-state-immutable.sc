// State management - immutable (stepping stone to FP)
val inc: (Int, Int) => (Int, Int) =
  (s, i) => (s + 1, i + 1)

val times2: (Int, Int) => (Int, Int) =
  (s, i) => (s + 1, i * 2)

val (s1, i1) = inc(0, 10)

val (s2, i2) = times2(s1, i1)

val (s3, i3) = times2(s2, i2)

// The above can be translated to be more FP via for comprehension - we'll see this right an introduction to State Monad