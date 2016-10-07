package main


case class MassPoint(mass: Double, pos: Vec2, velocity: Vec2, density: Double = 10) {
  def update(newPos:Vec2, newVelocity:Vec2) = MassPoint(mass, newPos, newVelocity)

  val radius = math.pow(mass/density,1.0/3)

  def gravityTo(target: MassPoint) = {
    val delta = pos - target.pos
    val dis = delta.magnitude
    if(dis<target.radius+radius)
      delta / triple(target.radius+radius) * mass
    else
      delta / triple(dis) * mass
  }

  def triple(x:Double) = x*x*x

  def momentum = velocity * mass
}


