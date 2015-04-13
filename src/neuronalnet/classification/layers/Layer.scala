package neuronalnet.classification.layers

import neuronalnet.classification.neurons.{BiasNeuron, Neuron}

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 *
 * Before u do anything u have to connect the layer via the connect method
 */
abstract class Layer(var units:Int) {
  var neurons:mutable.MutableList[Neuron] = mutable.MutableList[Neuron]()
  var postLayer:Layer = _

  def connectNextLayer(postLayer: Layer) = {
    this.postLayer = postLayer
  }


  createUnits()

  def updateWeights(errors:Array[Array[Double]], alpha:Double) = {
     for (i <- 0 until errors.length) {
      val connectionsSize = neurons.apply(i).postNeurons.length
      for (j <- 0 until connectionsSize) {
        val connection = neurons(i).postNeurons(j)
        connection.weight = connection.weight - alpha *errors(i)(j)
      }
    }


    /*for (i <- 0 until neurons.length) {
      for (j <- 0 until neurons.apply(i).postNeurons.length) {
        val newWeight = alpha*errors(i*j)


/*
        // we just have the connections to the bias units cuse to increase the activationvalue to fire the next neuron
        // but we have no weight to update on it. so overjump the bias neuron if our first connection is to one of it
        // every layer has a bias unit except the outputlayer
        if (connections.apply(0).postNeuron.isInstanceOf[BiasNeuron]){
          connections.apply(1).weight = connections.apply(1).weight - alpha*newWeight
        }else {
          connections.apply(0).weight = connections.apply(0).weight - alpha*newWeight
        }
        */
      }

    }


    for (i <- 0 until errors.length) {
      for (j <- 0 until neurons.apply(i).postNeurons.length){
        val connections = neurons.apply(i).postNeurons
        val newWeight = alpha*errors(i)
        // we just have the connections to the bias units cuse to increase the activationvalue to fire the next neuron
        // but we have no weight to update on it. so overjump the bias neuron if our first connection is to one of it
        // every layer has a bias unit except the outputlayer
        if (connections.apply(0).postNeuron.isInstanceOf[BiasNeuron]){
          connections.apply(1).weight = connections.apply(1).weight - alpha*newWeight
        }else {
          connections.apply(0).weight = connections.apply(0).weight - alpha*newWeight
        }
      }
    }
    */
  }

  def createUnits()

  /*
     create the connections between the neurons neurons
  */
  def connectPrevLayer(preLayer: Layer): Unit ={
      neurons.foreach(N => {
        preLayer.neurons.foreach(preNeuron => {
          preNeuron.register(N)
        })
      })
  }

  def getError():Array[Array[Double]] = {
    val error = Array.ofDim[Array[Double]](neurons.size)
    for (i <- 0 until error.length) {
      val connectionsSize = neurons.apply(i).postNeurons.length
      error(i) = Array.ofDim[Double](connectionsSize)
      for (j <- 0 until connectionsSize) {
        error(i)(j) = neurons.apply(i).postNeurons.apply(j).grad
      }
    }
    error
  }

}
