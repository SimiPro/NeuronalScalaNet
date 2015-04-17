package neuronalnet.classification.neurons

import ActivationWeight.ActivationWeight
import neuronalnet.classification.Application
import neuronalnet.classification.neurons.akka.Impuls

/**
 * Created by Simon on 11.04.2015.
 */
class InputNeuron(index:Int) extends Neuron(index) {

  override def trigger(newValue: Double, activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println("New Input: " + newValue)
    }
    postNeurons.foreach(N => {
      N ! Impuls(newValue)
    })
  }

  override def setValue(value: Double): Unit = {
    this.value = value
    this.finalValue = value
    trigger(value)
  }

}
