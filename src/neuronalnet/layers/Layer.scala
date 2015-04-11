package neuronalnet.layers

import neuronalnet.neurons.Neuron

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
abstract class Layer(units:Int) {
  var neurons:mutable.MutableList[Neuron] = mutable.MutableList[Neuron]()
  createUnits()

  def createUnits()


}
