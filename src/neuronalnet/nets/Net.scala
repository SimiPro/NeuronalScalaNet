package neuronalnet.nets


import neuronalnet.layers._
import neuronalnet.math_.MathHelper
import neuronalnet.trainingData.TrainSet

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
class Net(inputLayer: InputLayer, hiddenLayer: mutable.MutableList[HiddenLayer], outputLayer: OutputLayer) {
  connectLayers()

  def setResult(y: Int) = {
    outputLayer.neurons.apply(0).setResult(y)
  }

  def connectLayers():Unit = {
    hiddenLayer.apply(0).connectPrevLayer(inputLayer)
    var prevLayer = hiddenLayer.apply(0)
    // multi hiddenLayer handlin
    for (i <- 1 until hiddenLayer.length) {
      hiddenLayer(i).connectPrevLayer(prevLayer)
      prevLayer = hiddenLayer(i)
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

    inputLayer.updateWeights(inputError, alpha)
    hiddenLayer.updateWeights(hiddenError,alpha)

    J
  }

  def input(input1: Int, input2: Int): Unit = {
    // trigger bias with value 1
    inputLayer.neurons.apply(0).setValue(1)
    inputLayer.neurons.apply(1).setValue(input1)
    inputLayer.neurons.apply(2).setValue(input2)
  }

}

/**
 * Class to Build a net. Default 1 Input Layer, 1 Hidden Layer, 1 Output Layer
 * 2 Input Units, 1 Hidden Unit, 1 Output Unit
 * so just to make easy And and Or nets
 */
class NetBuilder {
  var inputLayerUnits = 2
  var inputLayer = 1

  var hiddenLayers = 1
  var hiddenLayerUnits = 1
  var hiddenLayers = mutable.MutableList[HiddenLayerBuilder]()

  var outputLayer = 1
  var outputLayerUnits = 1



  def init() = {

  }

  init()



  def setInputLayerUnits(units:Int):NetBuilder = {
    inputLayerUnits = units
    this
  }

  def addHiddenLayers(layer:HiddenLayerBuilder):NetBuilder = {
    hiddenLayers += layer
    this
  }

  def setOutputLayerUnits(units:Int):NetBuilder = {
    outputLayerUnits = units
    this
  }
  def setInputLayerUnits(units:Int):NetBuilder = {
    inputLayerUnits = units
    this
  }


  def build():Net = {
      var net:Net = new Net()

  }



}




}

