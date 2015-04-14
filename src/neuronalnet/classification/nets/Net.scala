package neuronalnet.classification.nets


import neuronalnet.classification.layers._
import neuronalnet.classification.trainingData.TrainSet

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
class Net(inputLayer: InputLayer, hiddenLayers: mutable.MutableList[HiddenLayer], outputLayer: OutputLayer) {


  def getResult():Array[Double] = {
    val result = Array.ofDim[Double](outputLayer.neurons.length)
    for (i <- 0 until result.length) {
      result(i) = outputLayer.neurons(i).value
    }
    result
  }

  connectLayers()

  def setResult(y: Array[Double]) = {
    if (y.length != outputLayer.units) {
      throw new Exception("Resultz from Y and neurons on outputlayer must be same size")
    }
    for (i <- 0 until y.length) {
      outputLayer.neurons(i).setResult(y(i))
    }
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
      val x = D.x
      val y = D.y
      input(x)
      val a = getResult()

      var cost = 0.0
      for (i <- 0 until a.size) {
        cost = cost + (-y(i))*math.log(a(i)) - (1 - y(i))*math.log(1-a(i))
      }

      //val cost = -D.y * math.log(a_3) - (1 - D.y) * math.log(1 - a_3)
      J = J + cost
    })
    J = J / trainData.size


    trainData.foreach(D => {
      input(D.x)
      setResult(D.y)
    })


    val alpha = 0.7


    inputLayer.updateWeightsLight(trainData.size, alpha)

    hiddenLayers.foreach(L => {
      L.updateWeightsLight(trainData.size, alpha)
    })


    J
  }

  def input(x:Array[Double]): Unit = {

    if (x.size != inputLayer.units) {
      throw new Exception("Feature Vector & InputNeurons have to be same size")
    }
    // trigger bias with value 1
    inputLayer.neurons.apply(0).setValue(1)

    for (i <- 1 to inputLayer.units) {
      inputLayer.neurons(i).setValue(x(i - 1))
    }

  }

  override def toString: String = {
    "Im a Neural Network Net \n" +
      "Input Layer: " + inputLayer.toString +
      "\n HiddenLayers: " + hiddenLayers.foreach(B => B)
    "Output Layer: " + outputLayer.toString
  }
}



