package neuronalnet.classification.neurons

import ActivationWeight.ActivationWeight
import neuronalnet.classification.Application

/**
 * Created by Simon on 11.04.2015.
 */
class InputNeuron extends Neuron {

  override def trigger(newValue:Double, activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println("New Input: " + newValue)
    }
    postNeurons.foreach(N =>  {
      N.postNeuron.trigger(newValue*N.weight)
    })
  }

  override def setValue(value: Double): Unit = {
    this.value = value
    this.finalValue = value
    trigger(value)
  }

}
