package neuronalnet.classification.neurons


import ActivationWeight.ActivationWeight
import neuronalnet.classification.Application
import neuronalnet.classification.math_.MathHelper

/**
 * Created by Simon on 11.04.2015.
 */
class HiddenNeuron extends Neuron {


  override def trigger(newValue:Double,activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)
    }

      // as soon as all connections pulled the trigger we can calculate forward
      // the first one resets the old value
      activationWeight match {
        case ActivationWeight.exeeded =>  {
          // sigmoid func
          increaseCurrentActivationValue()
          setValue(newValue)
          val sigValue = MathHelper.sigmoid(value)
          finalValue = sigValue
          // trigger next connections
          postNeurons.foreach(C => {
            C.postNeuron.trigger(sigValue*C.weight)
          })
        }
        case ActivationWeight.tolow => {
          setValue(newValue)
          increaseCurrentActivationValue()
        }
        case ActivationWeight.reset => {
          resetValues()
          increaseCurrentActivationValue()
          setValue(newValue)
        }
      }
  }

  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }


  override def setResult(delta:Double, weight: Double): Unit = {
    // set theta based on error from post neuron
    setError(delta*finalValue)

    val delta_2:Double = weight*delta*(finalValue*(1-finalValue))

    preNeurons.foreach(C => {
      C.setError(delta_2)
    })
  }
}
