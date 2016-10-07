package gui

import java.awt._
import java.awt.event._
import javax.swing.{JPanel, JTextField}
import javax.swing.event.{ChangeEvent, ChangeListener}

import main._

/**
 * Created by weijiayi on 8/15/15.
 */
class MainFormScala(mainForm: MainForm) {
  val lengthField = makeDoubleTextFiled(100, mainForm.timeField)
  val intervalField = makeDoubleTextFiled(0.05, mainForm.intervalField)
  val centerXField = makeDoubleTextFiled(0,mainForm.centerXField)
  val centerYField = makeDoubleTextFiled(0,mainForm.centerYField)
  val viewSizeField = makeDoubleTextFiled(400,mainForm.viewSizeField)
  val simplifyField = makeDoubleTextFiled(0.4,mainForm.simplifyField)
  val seedField = new ValueTextField[Int](2333,mainForm.seedField,_.toString,
    s=>try{Some(s.toInt)}catch{case _:Throwable => None},d=>needRecalc())

  def showCalculation = mainForm.showCalculationBox.isSelected
  def showGrids = mainForm.showGridBox.isSelected
  mainForm.showGridBox.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = canvasNeedRepaint()
  })

  var isSimulating = false
  mainForm.simButton.addActionListener(new ActionListener {
    override def actionPerformed(e: ActionEvent): Unit = 
      if(!isSimulating) simulate()
      else {
        finishedSimulation()
        recorderOpt.get.stop()
      }
  })

  mainForm.canvasHolder.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = ()

    override def keyPressed(e: KeyEvent): Unit = ()

    override def keyReleased(e: KeyEvent): Unit = {
      println("Key released!")
      val dir = e.getKeyCode match{
        case KeyEvent.VK_A => Vec2.left
        case KeyEvent.VK_D => Vec2.right
        case KeyEvent.VK_W => Vec2.up
        case KeyEvent.VK_S => Vec2.down
        case _ => Vec2.zero
      }
      val moveSpeed = 0.1
      val delta = dir * viewSizeField.value * moveSpeed
      centerXField.value += delta.x
      centerYField.value += delta.y
    }
  })

  var recorderOpt: Option[SimulationRecorder] = None
  var canvasOpt: Option[JPanel] = None
  
  val slider = mainForm.frameSlider
  slider.setEnabled(false)
  slider.setMajorTickSpacing(1)
  slider.setSnapToTicks(true)
  slider.addChangeListener(new ChangeListener {
    override def stateChanged(e: ChangeEvent): Unit = recorderOpt match {
      case Some(recorder) =>
        val maxTime = recorder.currentMaxTime
        val selectedTime = recorder.interval * slider.getValue
        mainForm.frameLabel.setText("%.2f/%.2f".format(selectedTime,maxTime))

        canvasNeedRepaint()
      case _ => ()
    }
  })


  def needRecalc():Unit = println("Need recalc")

  def makeDoubleTextFiled(init:Double, field:JTextField) ={
    new ValueTextField[Double](init,field,v=>"%.4g".format(v),
      s => try {Some(s.toDouble) }catch{ case _: Throwable => None },
      d => canvasNeedRepaint()
    )
  }

  def canvasNeedRepaint(): Unit = canvasOpt match {
    case Some(p) => p.repaint()
    case _ => ()
  }

  def simulate(): Unit ={
    isSimulating = true
    
    val sizeThreshHold = simplifyField.value
    val recorder = SimulationRecorder(intervalField.value, lengthField.value, sizeThreshHold)
    recorderOpt = Some(recorder)

    mainForm.simButton.setText("Stop")
    recorder.onFinished = ()=>finishedSimulation()

    canvasOpt = Some(GalaxyDrawer(recorder,this,pointColor = Color.blue,gridColor = Color.red, selectedColor = Color.green))

    mainForm.canvasHolder.removeAll()
    mainForm.canvasHolder.add(canvasOpt.get)

    val startTime = System.currentTimeMillis()
    recorder.simulateAPeriod(initGalaxy(), g=>{
      if(recorder.frameNum == 1) {
        slider.setEnabled(true)
      }
      slider.setMaximum(recorder.frameNum-1)
      val timePassed = System.currentTimeMillis()-startTime
      val speed = 1000.0*recorder.frameNum / timePassed
      mainForm.speedLabel.setText("%.2f frames/s".format(speed))
    })
  }
  
  def finishedSimulation(): Unit ={
    isSimulating = false
    mainForm.simButton.setText("Simulate")
  }

  def initGalaxy():Galaxy = {
    val size = 200
    def speedOfDis(dis: Double) = {
      math.atan(dis/size*4)*size*0.02
    }
    val seed = seedField.value
    val rand = new MyRandom(seed)
    val points = (0 until 2000).map(_=>{
      val dis = math.pow(rand.random.nextDouble(),1.5) * size
      val pos = rand.inUnitCircle().normalized * dis
      val v = Vec2(-pos.y,pos.x).normalized * speedOfDis(dis) + rand.inUnitCircle()*0.5
      MassPoint(5,pos,v)
    })
    Galaxy(points, time = 0)
  }

}
