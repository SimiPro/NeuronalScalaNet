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

  def costFunction(a_L:Array[Double], y:Array[Double]): Double = {
    var cost = 0.0
    for (i <- 0 until a_L.size) {
      cost = cost + (-y(i))*math.log(a_L(i)) - (1 - y(i))*math.log(1-a_L(i))
    }
    cost
  }

  def gradientChecking() = {

  }

  /**
   * Forward propagation through the net
   * The Result is saved after each forward prop on the output layer we haven't it available for later use
   * @param trainData
   * @return
   */
  def forwardProp(trainData:mutable.MutableList[TrainSet]): Double = {
    var J = 0.0
    trainData.foreach(D => {
      input(D.x)
      val a = getResult()
      //val cost = -D.y * math.log(a_3) - (1 - D.y) * math.log(1 - a_3)
      J = J + costFunction(a, D.y)
    })
    J / trainData.size
  }

  /**
   * BackProp. The gradient / error is summoned on each connection between the neurons.
   * @param trainData
   */
  def backprop(trainData:mutable.MutableList[TrainSet]): Unit ={
    var counter = 1
    trainData.foreach(D => {
      if (counter %100 == 0) {
        println("A: " + counter)
      }
      input(D.x)
      setResult(D.y)
      counter = counter + 1
    })
  }

  def train(trainData: mutable.MutableList[TrainSet]): Double = {
    val J = forwardProp(trainData)
    println("Cost: " + J)
    backprop(trainData)



    val alpha = 0.001

    gradientChecking()


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



