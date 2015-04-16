package neuronalnet.classification.neurons

import neuronalnet.classification.nets.{Net, TestNet}
import neuronalnet.classification.trainingData.TrainSet

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
case class Connection(postNeuron: Neuron, preNeuron: Neuron, var weight: Double, var grad: Double = 0.0) {
  /**
   * Gradient checking. Its to check if the gradient descent algo (~Backprop) is correctly implemented
   * @param trainData
   * @param net
   */
  def gradientCheck(trainData: mutable.MutableList[TrainSet], net: Net): Unit = {
    val epsilon: Double = 0.0001
    val wPlus = weight + epsilon
    val wMinus = weight - epsilon
    val actualWeight = weight

    weight = wPlus
    val jPlus = net.forwardProp(trainData)

    weight = wMinus
    val JMinus = net.forwardProp(trainData)



    val approx = (jPlus - JMinus) / (2 * epsilon)


    /*
    Lest just check the first 3 digits and
     */
    for (i <- 0 to 3) {

    }

    if (!(~=(approx, addition / trainData.size))) {
      throw new Exception("Grad from backprop should be around: " + approx + " But insteed its: " + grad)
    }

    weight = actualWeight

    /*
    Almost equal
     */
    def ~=(d: Double, d2: Double, precision: Double = 0.0001) = (d - d2).abs <= precision
  }


  def updateWeight(m: Int, alpha: Double): Unit = {
    this.weight = weight - alpha * (addition / m)
    addition = 0.0 //if we dont reset the addition we go strait to 0
  }

  var addition = 0.0

  def setError(delta: Double): Unit = {
    this.grad = delta * preNeuron.finalValue
    addition = addition + grad
    val newDelta = weight * delta * (preNeuron.finalValue * (1 - preNeuron.finalValue))
    preNeuron.preNeurons.foreach(C => {
      C.setError(newDelta)
    })
  }


  def costFunction(a_L: Array[Double], y: Array[Double]): Double = {
    var cost = 0.0
    for (i <- 0 until a_L.size) {
      cost = cost + (-y(i)) * math.log(a_L(i)) - (1 - y(i)) * math.log(1 - a_L(i))
    }
    cost
  }

}
