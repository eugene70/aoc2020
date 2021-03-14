package aoc2020

def time[R](name: String, fn: () => R): R = {
  val startMillis = System.currentTimeMillis()
  val result = fn()
  val endMillis = System.currentTimeMillis()
  println(s"$name result: $result, time: ${endMillis - startMillis} ms")
  result
}