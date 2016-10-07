package main

import scala.collection.mutable.ListBuffer

case class Galaxy(bodies: IndexedSeq[MassPoint], time:Double){

  val quadTree = formTree(bodies)

  def formTree(bodies: Seq[MassPoint]):QuadTree = {
    def isOneAxisLong(size: Vec2) = {
      if(size.x > 2*size.y) Some(0)
      else if (size.y > 2*size.x) Some(1)
      else None
    }

    if(bodies.length == 1)
      ExternalNode(bodies.head)
    else{
      val (minCorner,maxCorner) = findCorners(bodies)
      val size = maxCorner - minCorner
      val center = (minCorner+maxCorner)/2
      val (divideNum,indexFunc) = isOneAxisLong(size) match{
        case Some(axis) =>
          def indexInTwo(pos:Vec2) = if(pos.component(axis)<center.component(axis)) 0 else 1

          (2,(p:Vec2)=>indexInTwo(p))
        case None =>
          def indexInFour(pos:Vec2) = {
            val p1 = if(pos.x < center.x) 0 else 2
            val p2 = if(pos.y < center.y) 0 else 1
            p1 + p2
          }
          (4,(p:Vec2)=>indexInFour(p))
      }
      val subSpaces = (0 until divideNum).map(_=> new ListBuffer[MassPoint])
      bodies.foreach(b=>{
        val index = indexFunc(b.pos)
        subSpaces(index).+=(b)
      })
      val subTrees = subSpaces.filterNot(_.isEmpty).map(formTree)
      if(subTrees.length == 1) subTrees.head
      else InternalNode(subTrees,minCorner,maxCorner)
    }
  }

  def findCorners(points: Seq[MassPoint]) = {
    val head = points.head
    val headR = head.radius
    var minX = head.pos.x - headR
    var minY = head.pos.y - headR
    var (maxX,maxY) = (minX + 2*headR, minY+2*headR)
    points.tail.foreach(v=>{
      minX = math.min(minX,v.pos.x-v.radius)
      minY = math.min(minY,v.pos.y-v.radius)
      maxX = math.max(maxX,v.pos.x+v.radius)
      maxY = math.max(maxY,v.pos.y+v.radius)
    })
    (Vec2(minX,minY),Vec2(maxX,maxY))
  }

  def momentum = bodies.foldRight(Vec2.zero)((b,acc)=>acc+b.momentum)

  def collisionDetect(pos: Vec2): Option[MassPoint] = {
    def detectTree(quadTree: QuadTree): Option[MassPoint] = quadTree match {
      case ExternalNode(massPoint) =>
        if((pos - massPoint.pos).magnitudeSquared < massPoint.radius*massPoint.radius)
          Some(massPoint)
        else None
      case InternalNode(nodes,minCorner,maxCorner) =>
        if(minCorner.x<pos.x && minCorner.y < pos.y && maxCorner.x > pos.x && maxCorner.y > pos.y){
          nodes.map(t=> detectTree(t)).find(_.isDefined).map(_.get)
        } else None
    }

    detectTree(quadTree)
  }
}

trait QuadTree{
  def centerOfMass: Vec2

  def mass: Double
}

case class InternalNode(nodes:Seq[QuadTree], minCorner:Vec2, maxCorner:Vec2) extends QuadTree{

  override val mass: Double = nodes.foldRight(0.0)((n,acc)=>acc+n.mass)

  override val centerOfMass: Vec2 = {
    val weightedSum = nodes.foldRight(Vec2.zero)((node,acc)=>acc + node.centerOfMass * node.mass)
    weightedSum / mass
  }

  val maxEdge = (maxCorner-minCorner).maxComponent

  def gravityTo(delta:Vec2, dis:Double, target: MassPoint) = {
    def triple(x:Double) = x*x*x
    if(dis<target.radius+maxEdge/2)
      delta / triple(target.radius+maxEdge/2) * mass
    else
      delta / triple(dis) * mass
  }
}

case class ExternalNode(massPoint: MassPoint) extends QuadTree{
  override def centerOfMass: Vec2 = massPoint.pos

  override def mass: Double = massPoint.mass
}