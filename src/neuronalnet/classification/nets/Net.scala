package neuronalnet.classification.nets


import neuronalnet.classification.layers._
import neuronalnet.classification.math_.MathHelper
import neuronalnet.classification.trainingData.TrainSet

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
    outputLayer.neurons.apply(0).setResult(y, 0)
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

    var inputError = Array.ofDim[Array[Double]](inputLayer.neurons.size)  // inputlayersize * hiddenlayer size?
    val hiddenErrors = Array.ofDim[Array[Array[Double]]](hiddenLayers.size)
    // init hidden errors
    for (i <- 0 until hiddenLayers.size){
      val actualLayer = hiddenLayers(i)
      hiddenErrors(i) = Array.ofDim[Array[Double]](actualLayer.neurons.length)
      for (j <- 0 until actualLayer.neurons.length) {
        hiddenErrors(i)(j) = Array.ofDim[Double](actualLayer.neurons(j).postNeurons.size)
      }
       // mby hiddenlayersize * outputlayersize would work better
    }

    for (i <- 0 until inputLayer.neurons.length) {
      inputError(i) = Array.ofDim[Double](inputLayer.neurons(i).postNeurons.size)
    }


    //var hiddenError = Array.ofDim[hiddenLayers.size, Double](hiddenLayer.neurons.size) // hiddenlayersize * outputlayersize
    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)

      // Hidden Layer
      for (i <- 0 until hiddenLayers.size){
        val errorz = hiddenLayers(i).getError()
        for (j <- 0 until errorz.length) {
          hiddenErrors(i)(j) = MathHelper.cumulateArrays(hiddenErrors(i)(j),errorz(j))
        }
      }


      // Input Layer
      val gradPerLayer = inputLayer.getError()
      for (i <- 0 until gradPerLayer.length) {
        inputError(i) = MathHelper.cumulateArrays(inputError(i),gradPerLayer(i))
      }
       //
    })
    // learning rate alpha
    val alpha = 0.9

    inputError = MathHelper.divideEachElementInArray(inputError, trainData.size)


    for (i <- 0 until hiddenErrors.length) {
      hiddenErrors(i) = MathHelper.divideEachElementInArray(hiddenErrors(i), trainData.size)
    }

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

  override def toString: String = {
    "Im a Neural Network Net \n" +
    "Input Layer: " + inputLayer.toString +
    "\n HiddenLayers: " + hiddenLayers.foreach(B =>B )
    "Output Layer: " + outputLayer.toString
  }
}



