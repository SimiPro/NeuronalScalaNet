package neuronalnet.neurons

import neuronalnet.math_.MathHelper
import neuronalnet.Application
import ActivationWeight.ActivationWeight

/**
 * Created by Simon on 11.04.2015.
 */
class OutputNeuron extends Neuron  {
  var result:Double = 0


  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }

  override def trigger(newValue: Double,activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)
    }

    activationWeight match {
      case ActivationWeight.tolow => {
          increaseCurrentActivationValue()
          setValue(newValue)
      }
      case ActivationWeight.exeeded => {
        increaseCurrentActivationValue()
        setValue(newValue)
        value = MathHelper.sigmoid(value)
       // println(" Jepaa we finished forward Prop with result: " + result)
      }
      case ActivationWeight.reset => {
        resetValues()
        increaseCurrentActivationValue()
        setValue(newValue)
      }
    }
  }

  override def setResult(y: Double): Unit ={
    val delta = value - y
    setError(delta)

    preNeurons.foreach(C => {
      C.preNeuron.setResult(delta*C.weight)
    })
  }
}
