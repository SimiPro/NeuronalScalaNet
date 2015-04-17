package neuronalnet.classification.layers

import akka.actor.{ActorRef, Props}
import neuronalnet.classification.neurons.OutputNeuron
import neuronalnet.classification.neurons.akka.{ResultArray, ResultImpuls, OutputResult, RegisterResultListener}

/**
 * Created by Simon on 11.04.2015.
 */
class OutputLayer(units: Int) extends Layer(units) {
  var resultListener:ActorRef = _
  var counter = 0
  var result:Array[Double] = Array.ofDim[Double](units)

  override def registerResultListener(listener: ActorRef) = {
    resultListener = listener
  }

  override def addResult(result: Double, index: Int): Unit = {
    this.result(index) = result
    counter = counter + 1
    if (counter == units) {
      resultListener ! ResultArray(this.result)
      counter = 0
    }
  }

  override def createUnits(): Unit = {
    for (i <- 0 until units) {
      neurons += context.actorOf(Props(new OutputNeuron(i, this.self)))
    }
  }
}
