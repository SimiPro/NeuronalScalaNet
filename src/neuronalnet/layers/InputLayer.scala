package neuronalnet.layers

import neuronalnet.neurons.InputNeuron

/**
 * Created by Simon on 11.04.2015.
 */
class InputLayer(units:Int) extends Layer(units) {



  override def createUnits(): Unit = {
    // add bias neuron first of all
    // in case of input layer this is a normal inputNeuron which we fill with 1 everytime
    neurons += new InputNeuron
    for (x <- 1 to units) {
      neurons += new InputNeuron
    }
  }
}
