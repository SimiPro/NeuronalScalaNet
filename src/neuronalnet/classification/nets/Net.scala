package neuronalnet.classification.nets


import neuronalnet.classification.layers._
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

  def connectLayers(): Unit = {
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


  def train(trainData: mutable.MutableList[TrainSet]): Double = {
    var J = 0.0
    trainData.foreach(D => {
      input(D.x1, D.x2)
      val a_3 = outputLayer.getResult()
      val cost = -D.y * math.log(a_3) - (1 - D.y) * math.log(1 - a_3)
      J = J + cost
    })
    J = J / trainData.size


    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)
    })


    val alpha = 0.7


    inputLayer.updateWeightsLight(trainData.size, alpha)

    hiddenLayers.foreach(L => {
      L.updateWeightsLight(trainData.size, alpha)
    })


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
      "\n HiddenLayers: " + hiddenLayers.foreach(B => B)
    "Output Layer: " + outputLayer.toString
  }
}



