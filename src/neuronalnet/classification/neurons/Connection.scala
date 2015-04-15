package neuronalnet.classification.neurons

import neuronalnet.classification.nets.TestNet

/**
 * Created by Simon on 11.04.2015.
 */
case class Connection(postNeuron: Neuron, preNeuron:Neuron, var weight:Double, var grad:Double = 0.0) {
  def updateWeight(m: Int, alpha: Double): Unit = {
    this.weight = weight - alpha*(addition/m)
    addition = 0.0 //if we dont reset the addition we go strait to 0
  }

  var addition = 0.0

  def setError(delta: Double):Unit = {
    this.grad = delta*preNeuron.finalValue
    addition = addition + grad
    val newDelta = weight*delta*(preNeuron.finalValue*(1 - preNeuron.finalValue))
    preNeuron.preNeurons.foreach(C => {
      C.setError(newDelta)
    })
  }

  /*
  Not meant for Production just a test to look if backprop is working properly
   */
  def gradientChecking(weight:Double, epsilon:Double = 0.0001): Unit = {
    val wPlus = weight + epsilon
    val wMinus = weight - epsilon
    val gradApprox = (J(wPlus) - J(wMinus)) / (2*epsilon)

  }

  def J(theta:Double): Double = {
    TestNet.getTestNet().input()
  }

  def costFunction(a_L:Array[Double], y:Array[Double]): Double = {
    var cost = 0.0
    for (i <- 0 until a_L.size) {
      cost = cost + (-y(i))*math.log(a_L(i)) - (1 - y(i))*math.log(1-a_L(i))
    }
    cost
  }

}
