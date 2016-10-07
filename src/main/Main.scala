package main

/**
 * Created by weijiayi on 8/15/15.
 */
object Main {
  def main(args: Array[String]) {
    val init = Galaxy(IndexedSeq(MassPoint(1,Vec2.right,Vec2.up), MassPoint(2,Vec2.left,Vec2.down)), time = 0)
    val recorder = SimulationRecorder(0.1, 10, 0.6)
    recorder.simulateAPeriod(init,g=>println(g))
  }
}
