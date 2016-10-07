package main

import scala.util.Random

class MyRandom(seed: Int) {
  val random = new Random(seed)

  def inUnitCircle(): Vec2 = {
    val x = 2 * (random.nextDouble()-0.5)
    val y = 2 * (random.nextDouble()-0.5)
    if(x*x+y*y<1) Vec2(x,y)
    else inUnitCircle()
  }
}
