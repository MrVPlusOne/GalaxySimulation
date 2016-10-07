package main

import scala.collection.mutable.ArrayBuffer

/**
 * Created by weijiayi on 8/16/15.
 */
case class SimulationRecorder(interval: Double, length:Double, sizeThreshHold: Double){
  val maxFrameNum = (length / interval).toInt + 1
  var frameNum = 0
  val frames = new ArrayBuffer[Galaxy]()
  val simulator = new Simulator(sizeThreshHold)


  var onFinished = ()=>()

  def currentMaxTime = frameNum * interval

  def simulateAPeriod(initCondition: Galaxy, updatedGalaxy: Galaxy=>Unit): Unit = {
    new Thread(new Runnable {
      override def run(): Unit = {
        var current = initCondition
        def recordCurrent() = {
          frames += current
          frameNum += 1
          updatedGalaxy(current)
        }

        recordCurrent()
        for(i <- 0 until maxFrameNum){
          if(needStop){
            onFinished()
            return
          }
          current = simulator.simulate(current, interval)
          recordCurrent()
        }
        onFinished()
      }
    }).start()
  }

  var needStop = false
  def stop(): Unit ={
    needStop = true
  }
}
