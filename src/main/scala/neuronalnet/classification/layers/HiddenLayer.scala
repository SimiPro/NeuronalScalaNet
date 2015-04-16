package neuronalnet.classification.layers

import neuronalnet.classification.neurons.{BiasNeuron, HiddenNeuron}

/**
 * Created by Simon on 11.04.2015.
 */
class HiddenLayer(units: Int) extends Layer(units) {

  override def createUnits(): Unit = {
    neurons += new BiasNeuron
    for (x <- 1 to units) {
      neurons += new HiddenNeuron
    }
  }
}
