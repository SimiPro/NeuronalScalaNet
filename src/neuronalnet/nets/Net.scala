package neuronalnet.nets

import neuronalnet.layers.{HiddenLayer, InputLayer, OutputLayer}
import neuronalnet.trainingData.TrainSet

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
class Net {

  def setResult(y: Int) = {
    outputLayer.neurons.apply(0).setResult(y)
  }

  def train(trainData: mutable.MutableList[TrainSet]):Double = {
    var J = 0.0
    trainData.foreach(D => {
      input(D.x1, D.x2)
      val a_3 = outputLayer.getResult()
      val cost = -D.y*math.log(a_3) - (1 - D.y)*math.log(1 - a_3)
      J = J + cost
    })
    J = J / trainData.size

    val inputError = Array[Double](0.0,0.0,0.0)
    val hiddenError = Array[Double](0.0,0.0)
    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)

      // Hidden Layer
      hiddenError(0) = hiddenError(0) + hiddenLayer.neurons.apply(0).getError()
      hiddenError(1) = hiddenError(1) +  hiddenLayer.neurons.apply(1).getError()


      // Input Layer
      inputError(0) = inputError(0) + inputLayer.neurons.apply(0).getError()
      inputError(1) = inputError(1) + inputLayer.neurons.apply(1).getError()
      inputError(2) = inputError(2) + inputLayer.neurons.apply(2).getError()
      //
    })
    // learning rate alpha
    val alpha = 0.9



    // new weights
    for (i <- 0 until inputError.length) {
      val newWeight = inputError(i)/trainData.size
      val connections = inputLayer.neurons.apply(i).postNeurons
      connections.apply(1).weight = connections.apply(1).weight - alpha*newWeight
    }
    for (i <- 0 until hiddenError.length) {
      val newWeight = hiddenError(i)/trainData.size
      hiddenLayer.neurons.apply(i).postNeurons.apply(0).weight = hiddenLayer.neurons.apply(i).postNeurons.apply(0).weight - alpha*newWeight
    }

    J
  }

  def input(input1: Int, input2: Int): Unit = {
    // trigger bias with value 1
    inputLayer.neurons.apply(0).setValue(1)
    inputLayer.neurons.apply(1).setValue(input1)
    inputLayer.neurons.apply(2).setValue(input2)
  }

  var inputLayer = new InputLayer(2)
  var hiddenLayer = new HiddenLayer(1,inputLayer)
  var outputLayer = new OutputLayer(1,hiddenLayer)




}
