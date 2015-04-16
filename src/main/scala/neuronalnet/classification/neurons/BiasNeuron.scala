package neuronalnet.classification.neurons


import ActivationWeight.ActivationWeight
import neuronalnet.classification.Application

/**
 * Created by Simon on 11.04.2015.
 */
class BiasNeuron extends Neuron {
  this.value = 1
  this.finalValue = 1

  def receive = {

  }

  override def setValue(value: Double): Unit = {
    this.value = 1
    this.finalValue = 1
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
          C.postNeuron.trigger(1 * C.weight)
        })
      }
      case ActivationWeight.tolow => {
        increaseCurrentActivationValue()
      }
      case ActivationWeight.reset => {
        resetValues()
        value = 1
        finalValue = 1
        increaseCurrentActivationValue()
      }
    }
  }
}
