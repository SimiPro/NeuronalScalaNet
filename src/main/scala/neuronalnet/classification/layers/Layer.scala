package neuronalnet.classification.layers

import neuronalnet.classification.neurons.{BiasNeuron, Neuron}

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 *
 * Before u do anything u have to connect the layer via the connect method
 */
abstract class Layer(var units: Int) {
  var neurons: mutable.MutableList[Neuron] = mutable.MutableList[Neuron]()
  var postLayer: Layer = _

  def connectNextLayer(postLayer: Layer) = {
    this.postLayer = postLayer
  }


  createUnits()

  def updateWeightsLight(m: Int, alpha: Double): Unit = {
    neurons.foreach(N => {
      N.postNeurons.foreach(C => {
        C.updateWeight(m, alpha)
      })
    })
  }


  def createUnits()

  /*
     create the connections between the neurons neurons
  */
  def connectPrevLayer(preLayer: Layer): Unit = {
    neurons.foreach(N => {
      preLayer.neurons.foreach(preNeuron => {
        preNeuron.register(N)
      })
    })
  }

}
