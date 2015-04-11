package neuronalnet

import neuronalnet.nets.Net
import neuronalnet.trainingData.TrainSet
import scala.collection.mutable
import scala.util.Random

/**
 * Created by Simon on 09.04.2015.
 */
object Application {
  val DEBUG = false

  def main(args: Array[String]) {
    val neuronalNet = new Net()

    var trainData = new mutable.MutableList[TrainSet]
    for (i <- 1 to 100) {
      trainData += TrainSet(0,0,0)
    }
    for (i <- 1 to 100) {
      trainData += TrainSet(0,1,1)
    }
    for (i <- 1 to 100) {
      trainData += TrainSet(1,0,1)
    }
    for (i <- 1 to 100) {
      trainData += TrainSet(1,1,1)
    }

    for (i <- 1 to 2500) {
      val cost = neuronalNet.train(trainData)
      println("Iteration: " + i + " | Cost: " + cost)
    }

    neuronalNet.input(1,1)
    println("Input: 1 1 Output: " + neuronalNet.outputLayer.neurons.apply(0).value)
    neuronalNet.input(0,1)
    println("Input: 0 1 Output: " + neuronalNet.outputLayer.neurons.apply(0).value)
    neuronalNet.input(1,0)
    println("Input: 1 0 Output: " + neuronalNet.outputLayer.neurons.apply(0).value)
    neuronalNet.input(0,0)
    println("Input: 0 0 Output: " + neuronalNet.outputLayer.neurons.apply(0).value)


  }
}



object ActivationWeight extends Enumeration {
  type ActivationWeight = Value
  val exeeded,reset, tolow = Value
}



object MathHelper {
  val alpha = 0.9

  def sigmoidGradient(z: Double) = {
    sigmoid(z) * (1 - sigmoid(z))
  }

  var random:Random = new Random()

  /*
  Sigmoid of z
   */
  def sigmoid(value:Double) : Double = {
    val result =  1.0 / (1.0 + math.exp(-value))
    if (result < 0) {
      println("THIS FUCKING NOT ALLOWED TO HAPPEN")
    }
    result
  }

  /*
  Gets a number [-epsiolon;epsilon]
   */
  def randomInitializedNumber():Double = {
    val epsilon = 0.0001
    random.nextDouble()*2 *epsilon - epsilon
  }
}
