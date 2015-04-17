package neuronalnet.classification.layers

import akka.actor.{ActorSelection, ActorRef, ActorLogging, Actor}
import neuronalnet.classification.neurons.akka._


import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 *
 * Before u do something u have to connect the layer via the connect method
 */
abstract class Layer(var units: Int) extends Actor with ActorLogging {
  var neurons: mutable.MutableList[ActorRef] = mutable.MutableList[ActorRef]()
  var postLayer: ActorRef = _
  createUnits()

  def createUnits()


  def connectNextLayer(postLayer: ActorRef) = {
    this.postLayer = postLayer
  }

  /**
   *   preLayer.neurons.foreach(preNeuron => {
        preNeuron.register(PostNeuron)
      })
   *
   * @param postNeuron
   */
  def connectPostNeuron(postNeuron: ActorRef): Unit = {
    neurons.foreach(N => {
      N ! ConnectPostNeuron(postNeuron)
    })
  }

  def input(x: Array[Double]): Unit = {
    // trigger bias unit first
    for (i <- 1 to x.length) {
      neurons(i) ! Impuls(x(i))
    }
    //neurons.foreach(N => N ! Impuls())
  }

  def registerResultListener(ref: ActorRef): Unit = {}

  def addResult(d: Double, i: Int): Unit = {}

  def receive = {
    case ConnectPostLayer(postLayer) => connectNextLayer(postLayer)
    case ConnectPreLayer(preLayer) => connectPrevLayer(preLayer)
    case ConnectPostNeuron(postNeuron) => connectPostNeuron(postNeuron)
    case ImpulsArray(x) => input(x)
    case RegisterResultListener(listener:ActorRef) => registerResultListener(listener)
    case ResultImpuls(result,index) => addResult(result, index)
    case x => log.info("Unkown Message: {} ", x )

  }



  def updateWeightsLight(m: Int, alpha: Double): Unit = {
    neurons.foreach(N => {
      N ! UpdateWeights(m, alpha)
      /*
      N.postNeurons.foreach(C => {
        C.updateWeight(m, alpha)
      })
      */
    })
  }




  /*
     create the connections between the neurons neurons
     We dont like so hardcoded connections
  */
  def connectPrevLayer(preLayer: ActorRef): Unit = {
    neurons.foreach(postNeuron => {
      preLayer ! ConnectPostNeuron(postNeuron)
    })
  }

}
