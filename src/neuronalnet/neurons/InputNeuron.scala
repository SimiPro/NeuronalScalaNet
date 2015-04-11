package neuronalnet.neurons

import neuronalnet.ActivationWeight.ActivationWeight
import neuronalnet.Application

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
    trigger(value)
  }

  override def setResult(error: Double): Unit = {
    setError(error*value)

 //   postNeurons.apply(0).weight = postNeurons.apply(0).weight - MathHelper.alpha*getError()

  }
}
