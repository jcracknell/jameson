package jameson
package examples

import java.time._

trait Benchmark extends App {
  def iterations: Int

  def benchmark(description: String)(op: => Unit): Unit = {
    println(s"[$description]")
    println("- Warming up...")
    var i = 0
    while(i < iterations) {
      op
      i += 1
    }

    val start = System.nanoTime()
    i = 0
    while(i < iterations) {
      op
      i += 1
    }

    val elapsed = (System.nanoTime() - start).toDouble / 1000000d
    println(s"- Completed $iterations iterations in ${elapsed}ms.")
  }
}
