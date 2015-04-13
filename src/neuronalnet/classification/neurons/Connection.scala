package neuronalnet.classification.neurons

/**
 * Created by Simon on 11.04.2015.
 */
case class Connection(postNeuron: Neuron, preNeuron:Neuron, var weight:Double, var grad:Double = 0.0) {

  def setError(delta: Double):Unit = {
    this.grad = delta*preNeuron.finalValue
    val newDelta = weight*delta*(preNeuron.finalValue*(1 - preNeuron.finalValue))
    preNeuron.preNeurons.foreach(C => {
      C.setError(newDelta)
    })
  }
}
