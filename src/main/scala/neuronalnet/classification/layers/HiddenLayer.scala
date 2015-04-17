package neuronalnet.classification.layers

import akka.actor.{ActorRef, Props}
import neuronalnet.classification.neurons.akka.{RegisterResultListener, Impuls}
import neuronalnet.classification.neurons.{InputNeuron, BiasNeuron, HiddenNeuron}

/**
 * Created by Simon on 11.04.2015.
 */
class HiddenLayer(units: Int) extends Layer(units) {


  override def input(x:Array[Double]): Unit = {
    // trigger bias unit first
    neurons(0) ! Impuls(1)

    for (i <- 1 to x.length) {
      neurons(i) ! Impuls(x(i - 1))
    }
  }

  override def createUnits(): Unit = {
    neurons += context.actorOf(Props(new BiasNeuron))
    for (i <- 1 to units) {
      neurons += context.actorOf(Props(new HiddenNeuron(i)))
    }
  }
}
