package neuronalnet.classification


import neuronalnet.classification.nets.NetBuilder
import neuronalnet.classification.trainingData.TrainSet


import scala.collection.mutable

/**
 * Created by Simon on 09.04.2015.
 */
object Application {
  val DEBUG = false

  def main(args: Array[String]) {


    var trainData = new mutable.MutableList[TrainSet]
    for (i <- 1 to 100) {
      trainData += TrainSet(0,0,1)
    }
    for (i <- 1 to 100) {
      trainData += TrainSet(0,1,0)
    }
    for (i <- 1 to 100) {
      trainData += TrainSet(1,0,0)
    }
    for (i <- 1 to 100) {
      trainData += TrainSet(1,1,1)
    }


    var cost = 0.1
    var i = 1
    var neuronalNet = new NetBuilder().build()
    while(cost > 0.09) {
      println("Try: " + i)
      i = i +1
      neuronalNet = new NetBuilder().build()
      for (i <- 1 to 2500) {
        cost = neuronalNet.train(trainData)
        println("Iteration: " + i + " | Cost: " + cost)
      }
    }

    neuronalNet.input(1,1)
    println("Input: 1 1 Output: " + neuronalNet.getResult())
    neuronalNet.input(0,1)
    println("Input: 0 1 Output: " + neuronalNet.getResult())
    neuronalNet.input(1,0)
    println("Input: 1 0 Output: " + neuronalNet.getResult())
    neuronalNet.input(0,0)
    println("Input: 0 0 Output: " + neuronalNet.getResult())


  }
}
