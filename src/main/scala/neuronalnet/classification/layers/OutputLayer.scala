package neuronalnet.classification.layers

import neuronalnet.classification.neurons.OutputNeuron

/**
 * Created by Simon on 11.04.2015.
 */
class OutputLayer(units: Int) extends Layer(units) {


  override def createUnits(): Unit = {
    for (x <- 1 to units) {
      neurons += new OutputNeuron
    }
  }
}
