package neuronalnet.nets


import neuronalnet.layers.{HiddenLayer, InputLayer, OutputLayer}
import neuronalnet.math_.MathHelper
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

    var inputError = Array.ofDim[Double](inputLayer.neurons.size*hiddenLayer.units)  // inputlayersize * hiddenlayer size?
    var hiddenError = Array.ofDim[Double](hiddenLayer.neurons.size) // hiddenlayersize * outputlayersize
    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)

      // Hidden Layer
      hiddenError = MathHelper.cumulateArrays(hiddenError, hiddenLayer.getError())

      // Input Layer
      inputError = MathHelper.cumulateArrays(inputError, inputLayer.getError())
       //
    })
    // learning rate alpha
    val alpha = 0.9

    inputError = MathHelper.divideEachElementInArray(inputError, trainData.size)
    hiddenError = MathHelper.divideEachElementInArray(hiddenError, trainData.size)

    //inputLayer.updateWeights(inputError, alpha)
    //hiddenLayer.updateWeights(hiddenError,alpha)



    // new weights
    for (i <- 0 until inputError.length) {
      val newWeight = inputError(i)
      val connections = inputLayer.neurons.apply(i).postNeurons
      connections.apply(1).weight = connections.apply(1).weight - alpha*newWeight
    }
    for (i <- 0 until hiddenError.length) {
      val newWeight = hiddenError(i)
      val connections = hiddenLayer.neurons.apply(i).postNeurons
      connections.apply(0).weight = connections.apply(0).weight - alpha*newWeight
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
