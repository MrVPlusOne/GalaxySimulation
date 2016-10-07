package main

class Simulator(threshHold: Double) {

  def simulate(galaxy: Galaxy, deltaTime: Double): Galaxy = {
    val newBodies = galaxy.bodies.par.map(body =>{
      val force = calcAcc(galaxy.quadTree, body)
      val newV = body.velocity + force * deltaTime
      val newP = body.pos + newV * deltaTime
      body.update(newP, newV)
    }).toIndexedSeq
    Galaxy(newBodies,galaxy.time+deltaTime)
  }

  def calcAcc(tree:QuadTree, body: MassPoint):Vec2 = calcAcc(tree,body,node=>())
  
  def calcAcc(tree:QuadTree, body: MassPoint, useSimplification: InternalNode=>Unit): Vec2 = {
    val delta = tree.centerOfMass - body.pos
    val dis = delta.magnitude
    tree match{
      case ExternalNode(point) =>
        point.gravityTo(body)
      case node @ InternalNode(nodes,_,_) =>
        if(node.maxEdge < threshHold * dis){
          useSimplification(node)
          node.gravityTo(delta,dis,body)
        }else{
          nodes.foldRight(Vec2.zero)((tree,acc)=>acc + calcAcc(tree,body, useSimplification))
        }
    }
  }
}


