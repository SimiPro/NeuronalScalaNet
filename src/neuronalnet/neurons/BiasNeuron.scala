package neuronalnet.neurons

import ActivationWeight.ActivationWeight
import ActivationWeight.ActivationWeight
import neuronalnet.Application

/**
 * Created by Simon on 11.04.2015.
 */
class BiasNeuron extends Neuron {
  this.value = 1

  override def setValue(value: Double): Unit = {
    this.value = 1
    trigger(1)
  }

  override def trigger(newValue: Double, activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)

    }

    activationWeight match {
      case ActivationWeight.exeeded => {
        increaseCurrentActivationValue()
        // trigger next connections
        postNeurons.foreach(C => {
          // cause im a bias 1*
          C.postNeuron.trigger(1*C.weight)
        })
      }
      case ActivationWeight.tolow => {
        increaseCurrentActivationValue()
      }
      case ActivationWeight.reset => {
        resetValues()
        value = 1
        increaseCurrentActivationValue()
      }
    }
  }

  override def setResult(error: Double): Unit = {
    // bias unit has no delta but an error value to correct
    // TODO: error = delta_3 * weight but we need just delta_3 change this
    setError(postNeurons.apply(0).postNeuron.getError()*1)
   // postNeurons.apply(0).weight = postNeurons.apply(0).weight - MathHelper.alpha*getError()

  }
}