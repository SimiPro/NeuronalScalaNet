package neuronalnet.classification

import scala.collection.mutable

/**
 * Created by Simon on 10.04.2015.
 */
object Test {

  case class Connection(var postNeuron: Neuron, var preNeuron:Neuron, var weight:Double)

  class Neuron {
    var value = 0
    var connections = new mutable.MutableList[Connection]

    def updateWeight(value:Int): Unit ={
      this.value = value
      connections.apply(0).weight = 5
      connections.apply(0).postNeuron.value = 7
    }
  }

  def main(args: Array[String]) {
    val neuron1 = new Neuron
    val neuron2 = new Neuron
    var connection = new Connection(neuron2, neuron1, 0.0)
    neuron1.connections += connection
    neuron2.connections += connection


    neuron1.updateWeight(2)

    println("And my new value should be 5: " + neuron2.connections.apply(0).weight)
    println("My value should be 7: " + neuron2.value)







  }
}
