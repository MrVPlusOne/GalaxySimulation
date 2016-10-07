package gui

import java.awt.event.{MouseMotionAdapter, MouseEvent, MouseAdapter}
import java.awt._
import javax.swing.JPanel

import main._

/**
 * Created by weijiayi on 8/16/15.
 */
case class GalaxyDrawer(recorder: SimulationRecorder, mainForm: MainFormScala,
                        pointColor: Color, gridColor: Color, selectedColor: Color) extends JPanel{

  setBackground(Color.white)
  var galaxyShowing: Option[Galaxy] = None
  var selectedBody: Option[MassPoint] = None

  var mouseLastPos: Option[Vec2] = None
  this.addMouseListener(new MouseAdapter {
    override def mouseReleased(e: MouseEvent): Unit = {
      mouseLastPos = None
      galaxyShowing match{
        case Some(galaxy)=>
          val pos = pointToGalaxy(e.getPoint)
          val newlySelected = galaxy.collisionDetect(pos)
          if(newlySelected != selectedBody){
            selectedBody = newlySelected
            repaint()
          }
        case None => ()
      }
    }
    override def mousePressed(e: MouseEvent): Unit = {
      mouseLastPos = Some(Vec2(e.getX,e.getY))
    }
  })

  this.addMouseMotionListener(new MouseMotionAdapter {
    override def mouseDragged(e: MouseEvent): Unit = {
      mouseLastPos match{
        case Some(lastP)=>
          val currentPos = Vec2(e.getX,e.getY)
          val delta = (lastP - currentPos)*mainForm.viewSizeField.value / getWidth
          mainForm.centerXField.value += delta.x
          mainForm.centerYField.value += delta.y
          mouseLastPos = Some(currentPos)
        case None => ()
      }
    }
  })
  
  var (xMin,yMin,pixelPerLength) = (0.0,0.0,0.0)
  
  def galaxyToPoint(pos:Vec2) = {
    val x = ((pos.x - xMin) * pixelPerLength).toInt
    val y = ((pos.y - yMin) * pixelPerLength).toInt
    (x,y)
  }
  
  def pointToGalaxy(point:Point) = {
    val x = point.x / pixelPerLength + xMin
    val y = point.y / pixelPerLength + yMin
    Vec2(x,y)
  }
  
  def setTransform(center: Vec2, viewSize:Double, canvasSize: Dimension): Unit ={
    xMin = center.x - viewSize/2
    yMin = center.y - viewSize*canvasSize.height/canvasSize.width/2
    pixelPerLength = canvasSize.width.toDouble / viewSize
  }
  
  private def drawGalaxy(g: Graphics2D, galaxy: Galaxy, showGrids:Boolean, showCalculation:Boolean): Unit = {

    def drawInternalNode(n: InternalNode): Unit ={
      val (x,y) = galaxyToPoint(n.minCorner)
      val size = (n.maxCorner-n.minCorner)*pixelPerLength
      g.drawRect(x,y,size.x.toInt,size.y.toInt)
    }
    def drawQuadTree(quadTree: QuadTree): Unit = quadTree match{
      case n: InternalNode=> {
        drawInternalNode(n)
        n.nodes.foreach(n=>drawQuadTree(n))
      }
      case _=> ()
    }
    def drawMassPoint(b: MassPoint): Unit ={
      val x = ((b.pos.x - xMin - b.radius) * pixelPerLength).toInt
      val y = ((b.pos.y - yMin - b.radius) * pixelPerLength).toInt
      val radius = (b.radius * 2 * pixelPerLength).toInt
      g.fillOval(x,y,radius,radius)
    }

    if(showGrids){
      g.setColor(gridColor)
      drawQuadTree(galaxy.quadTree)
    }

    g.setColor(pointColor)
    galaxy.bodies.foreach(drawMassPoint)

    g.setColor(selectedColor)
    selectedBody.foreach(b=>{
      drawMassPoint(b)
      if(showCalculation){
        recorder.simulator.calcAcc(galaxy.quadTree,b,drawInternalNode)
      }
    })

    val momentumText = "CM Velocity: " + (galaxy.momentum / galaxy.quadTree.mass).prettyPrint
    g.setColor(Color.black)
    g.drawString(momentumText,20,20)
    
    if(!galaxyShowing.contains(galaxy)) {
      galaxyShowing = Some(galaxy)
      selectedBody = None
    }
  }

  override def paint(g: Graphics): Unit = {
    super.paint(g)
    if (recorder.frameNum > 0) {
      val g2d = g.asInstanceOf[Graphics2D]
      val galaxy = recorder.frames(mainForm.slider.getValue)
      val center = Vec2(mainForm.centerXField.value, mainForm.centerYField.value)
      setTransform(center,mainForm.viewSizeField.value,getSize)
      drawGalaxy(g2d, galaxy, mainForm.showGrids, mainForm.showCalculation)
    }
  }

}
