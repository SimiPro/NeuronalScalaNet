package neuronalnet.classification


import java.util.concurrent.TimeUnit

import akka.actor.ActorRef
import akka.util.Timeout
import neuronalnet.classification.extern.filereader.MNISTReader
import neuronalnet.classification.nets.NetBuilder
import neuronalnet.classification.neurons.akka.{ImpulsArray, Impuls, Train}
import neuronalnet.classification.trainingData.TrainSet
import akka.pattern.ask
import scala.concurrent.duration._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global



import scala.collection.mutable


/**
 * Created by Simon on 09.04.2015.
 */
object Application {
  implicit val timout:Timeout = Timeout(30, TimeUnit.SECONDS)


  val DEBUG = false

  def main(args: Array[String]) {

    var trainData = new mutable.MutableList[TrainSet]
    for (i <- 1 to 5) {
      trainData += TrainSet(Array[Double](0, 0), Array[Double](1))
    }
    for (i <- 1 to 5) {
      trainData += TrainSet(Array[Double](1, 0), Array[Double](0))
    }
    for (i <- 1 to 5) {
      trainData += TrainSet(Array[Double](0, 1), Array[Double](0))
    }
    for (i <- 1 to 5) {
      trainData += TrainSet(Array[Double](1, 1), Array[Double](1))
    }


    var cost = 0.1
    var i = 1
    var neuronalNet:ActorRef = null
    while (cost > 0.09) {
      println("Try: " + i)
      i = i + 1
      neuronalNet = new NetBuilder().build()
      for (i <- 1 to 2500) {

        val cost = ask(neuronalNet, Train(trainData)).mapTo[Double]
        Await.result(cost, 30 seconds)

        println("Iteration: " + i + " | Cost: " + cost.map(D => {
          D
        }))
      }
    }

    val result =  ask(neuronalNet,ImpulsArray(Array[Double](1, 1))).mapTo[Array[Double]]
   /* println("Input: 1 1 Output: " + neuronalNet.getResult()(0))
    neuronalNet ! ImpulsArray(Array[Double](0, 1))
    println("Input: 0 1 Output: " + neuronalNet.getResult()(0))
    neuronalNet ! ImpulsArray(Array[Double](1, 0))
    println("Input: 1 0 Output: " + neuronalNet.getResult()(0))
    neuronalNet ! ImpulsArray(Array[Double](0, 0))
    println("Input: 0 0 Output: " + neuronalNet.getResult()(0))
    */


  }
}
