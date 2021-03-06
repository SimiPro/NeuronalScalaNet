package neuronalnet.classification.neurons


import ActivationWeight.ActivationWeight
import neuronalnet.classification.Application
import neuronalnet.classification.math_.MathHelper
import neuronalnet.classification.neurons.akka.Impuls

/**
 * Created by Simon on 11.04.2015.
 */
class HiddenNeuron(index:Int) extends Neuron(index) {


  override def trigger(newValue: Double, activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)
    }

    // as soon as all connections pulled the trigger we can calculate forward
    // the first one resets the old value
    activationWeight match {
      case ActivationWeight.exeeded => {
        // sigmoid func
        increaseCurrentActivationValue()
        setValue(newValue)
        val sigValue = MathHelper.sigmoid(value)
        finalValue = sigValue
        // trigger next connections
        postNeurons.foreach(C => {
          C ! Impuls(sigValue)
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

}
