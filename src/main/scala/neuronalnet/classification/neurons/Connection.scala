package neuronalnet.classification.neurons

import java.util.concurrent.TimeUnit

import _root_.akka.actor.{ActorLogging, Actor, ActorRef}
import _root_.akka.util.Timeout
import neuronalnet.classification.nets.{Net, TestNet}
import neuronalnet.classification.neurons.akka.{Impuls, GetFinalValue, DeltaImpuls}
import neuronalnet.classification.trainingData.TrainSet
import _root_.akka.pattern.ask


import scala.collection.mutable
import scala.concurrent.Future


/**
 * Created by Simon on 11.04.2015.
 */
class Connection(postNeuron: ActorRef, preNeuron: ActorRef, var weight: Double, var grad: Double = 0.0) extends Actor with ActorLogging {
  implicit val timout:Timeout = Timeout(3, TimeUnit.SECONDS)
  import context.dispatcher
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


    if (!(~=(approx, addition / trainData.size))) {
      throw new Exception("Grad from backprop should be around: " + approx + " But insteed its: " + grad)
    }

    weight = actualWeight

    /*
    Almost equal
     */
    def ~=(d: Double, d2: Double, precision: Double = 0.0001) = (d - d2).abs <= precision
  }

  def impuls(value: Double): Unit = {
    postNeuron ! Impuls(value*weight)
  }

  def receive = {
    case Impuls(value) => impuls(value)
    case DeltaImpuls(delta) => deltaImpuls(delta)
    case x => log.info("Unkown Message: {} ", x )
  }

  def deltaImpuls(delta:Double): Unit = {

  }

  def updateWeight(m: Int, alpha: Double): Unit = {
    this.weight = weight - alpha * (addition / m)
    addition = 0.0 //if we dont reset the addition we go strait to 0
  }

  var addition = 0.0

  def setError(delta: Double): Unit = {
    val future: Future[Double] = ask(preNeuron, GetFinalValue).mapTo[Double]
    future.map(A => {
      this.grad = delta * A
      addition = addition + grad
      val newDelta = weight * delta * (A * (1 - A))
      preNeuron ! DeltaImpuls(newDelta)
      /*
      preNeuron.preNeurons.foreach(C => {
        C.setError(newDelta)
      })
      */
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
