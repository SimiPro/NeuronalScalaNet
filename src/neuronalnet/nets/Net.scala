package neuronalnet.nets


import neuronalnet.layers._
import neuronalnet.math_.MathHelper
import neuronalnet.trainingData.TrainSet

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
class Net(inputLayer: InputLayer, hiddenLayers: mutable.MutableList[HiddenLayer], outputLayer: OutputLayer) {


  def getResult() = {
    outputLayer.neurons.apply(0).value
  }

  connectLayers()

  def setResult(y: Int) = {
    outputLayer.neurons.apply(0).setResult(y)
  }

  def connectLayers():Unit = {
    hiddenLayers.apply(0).connectPrevLayer(inputLayer)
    inputLayer.connectNextLayer(hiddenLayers.apply(0))

    var prevLayer = hiddenLayers.apply(0)
    // multi hiddenLayer handlin
    for (i <- 1 until hiddenLayers.length) {
      hiddenLayers(i).connectPrevLayer(prevLayer)
      prevLayer = hiddenLayers(i)
    }
    outputLayer.connectPrevLayer(prevLayer)
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

    var inputError = Array.ofDim[Double](inputLayer.neurons.size*inputLayer.postLayer.units)  // inputlayersize * hiddenlayer size?
    var hiddenErrors = Array.ofDim[Array[Double]](hiddenLayers.size)
    // init hidden errors
    for (i <- 0 until hiddenLayers.size){
      hiddenErrors(i) = Array.ofDim[Double](hiddenLayers(i).neurons.size) // mby hiddenlayersize * outputlayersize would work better
    }


    //var hiddenError = Array.ofDim[hiddenLayers.size, Double](hiddenLayer.neurons.size) // hiddenlayersize * outputlayersize
    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)

      // Hidden Layer
      for (i <- 0 until hiddenLayers.size){
        val hiddenLayer = hiddenLayers.apply(i)
        hiddenErrors(i) = MathHelper.cumulateArrays(hiddenErrors(i), hiddenLayer.getError())
      }


      // Input Layer
      inputError = MathHelper.cumulateArrays(inputError, inputLayer.getError())
       //
    })
    // learning rate alpha
    val alpha = 0.9

    inputError = MathHelper.divideEachElementInArray(inputError, trainData.size)
    hiddenErrors = MathHelper.divideEachElementInArray(hiddenErrors, trainData.size)

    inputLayer.updateWeights(inputError, alpha)

    for (i <- 0 until hiddenLayers.length) {
      hiddenLayers(i).updateWeights(hiddenErrors(i), alpha)
    }

    J
  }

  def input(input1: Int, input2: Int): Unit = {
    // trigger bias with value 1
    inputLayer.neurons.apply(0).setValue(1)
    inputLayer.neurons.apply(1).setValue(input1)
    inputLayer.neurons.apply(2).setValue(input2)
  }

}



