package neuronalnet.classification.layers

import akka.actor.Props
import neuronalnet.classification.neurons.InputNeuron
import neuronalnet.classification.neurons.akka.Impuls

/**
 * Created by Simon on 11.04.2015.
 */
class InputLayer(units: Int) extends Layer(units) {

  override def input(x:Array[Double]): Unit = {
    // trigger bias unit first
    neurons(0) ! Impuls(1)

    for (i <- 1 to x.length) {
      neurons(i) ! Impuls(x(i - 1))
    }
  }


  override def createUnits(): Unit = {
    // add bias neuron first of all
    // in case of input layer this is a normal inputNeuron which we fill with 1 everytime
    neurons += context.actorOf(Props(new InputNeuron(0)))
    for (i <- 1 to units) {
      neurons += context.actorOf(Props(new InputNeuron(i)))
    }
  }
}
