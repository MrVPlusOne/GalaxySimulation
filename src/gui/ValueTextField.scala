package gui

import java.awt.event.{FocusEvent, FocusListener, KeyEvent, KeyListener}
import javax.swing.JTextField

/**
 * Created by weijiayi on 4/21/15.
 */
class ValueTextField[T](init:T,val textField:JTextField,
                        value2Text:T=>String, text2Value:String=>Option[T],valueChanged:T=>Unit) {
  var _value = init
  textField.setText(value2Text(value))

  def value = _value
  def value_= (v:T): Unit ={
    if(v!=_value){
      _value = v
      textField.setText(value2Text(v))
      valueChanged(v)
    }
  }

  textField.addFocusListener(new FocusListener {
    override def focusGained(e: FocusEvent): Unit = ()

    override def focusLost(e: FocusEvent): Unit = {
      whenFocusLost()
    }
  })

  def whenFocusLost(): Unit ={
    val text = textField.getText
    text2Value(text) match{
      case Some(v)=> value = v
      case None => textField.setText(value2Text(value))
    }
  }

  textField.addKeyListener(new KeyListener {
    override def keyTyped(e: KeyEvent): Unit = ()
    override def keyPressed(e: KeyEvent): Unit = ()
    override def keyReleased(e: KeyEvent): Unit = {
      if(e.getKeyCode==KeyEvent.VK_ENTER){
        whenFocusLost()
      }
    }
  })
}
