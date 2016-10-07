package main


case class Vec2(x:Double, y:Double) {

  def * (s: Double) = Vec2(x*s,y*s)

  def / (s: Double) = Vec2(x/s,y/s)


  def + (v: Vec2) = Vec2(x+v.x,y+v.y)

  def - (v: Vec2) = Vec2(x-v.x,y-v.y)

  def magnitudeSquared = x*x + y*y

  lazy val magnitude = math.sqrt(magnitudeSquared)

  def normalized = this / magnitude

  def component(i: Int) = if(i==0) x else y

  def maxComponent = math.max(x,y)

  def prettyPrint = "(%.3f, %.3f)".format(x,y)

}

object  Vec2{
  val zero = Vec2(0,0)
  val (right, left, up, down) = (Vec2(1,0), Vec2(-1,0), Vec2(0,1), Vec2(0,-1))
}
