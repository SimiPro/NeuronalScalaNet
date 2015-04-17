package neuronalnet.classification.nets

import akka.actor._
import neuronalnet.classification.neurons.akka._
import neuronalnet.classification.trainingData.TrainSet
import scala.concurrent.duration._


import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
class Net(inputLayer: ActorRef, hiddenLayers: mutable.MutableList[ActorRef], outputLayer: ActorRef, outputLayerUnits:Int) extends Actor with ActorLogging {
  var resultArray = Array.ofDim[Double](outputLayerUnits)


  connectLayers()
  registerResultListener()

  def getResult(): Array[Double] = {
    resultArray
  }

  def receive = {
    case ResultArray(result) => this.resultArray = result
    case Train(set) => train(set)
    case x => log.info("Unkown Message: {} ", x )
  }


  def registerResultListener() : Unit = {
    outputLayer ! RegisterResultListener(context.self)
  }




  def connectLayers() = {
    // akka://net/outputlayer/unit(i)  = OutputNeuron(i)
    // akka://net/hiddenlayer/unit(i) = HiddenNeuron(i)
    // akka://net/inputLayer/unit(i) = InputNeuron(i)
    var postHiddenLayer = hiddenLayers(0)
    var preHiddenLayer:ActorRef = null


    inputLayer ! ConnectPostLayer(postHiddenLayer)
    postHiddenLayer ! ConnectPreLayer(inputLayer)
    for (j <- 1 until hiddenLayers.length) {
      val L = hiddenLayers(j)
        for (i <- 1 until hiddenLayers.length) {
          preHiddenLayer = postHiddenLayer
          postHiddenLayer = hiddenLayers(i)

          preHiddenLayer ! ConnectPostLayer(postHiddenLayer)
          postHiddenLayer ! ConnectPreLayer(preHiddenLayer)
        }

    }
    postHiddenLayer ! ConnectPostLayer(outputLayer)
    outputLayer ! ConnectPreLayer(postHiddenLayer)
    Thread.sleep(3000)
  }

  def setResult(y: Array[Double]) = {
    outputLayer ! ResultArray(y)
  }


  def costFunction(a_L: Array[Double], y: Array[Double]): Double = {
    var cost = 0.0
    for (i <- 0 until a_L.size) {
      cost = cost + (-y(i)) * math.log(a_L(i)) - (1 - y(i)) * math.log(1 - a_L(i))
    }
    cost
  }

  /**
   * Checks the gradient on the connections calculated by backprop
   * @param trainData
   */
  def gradientChecking(trainData: mutable.MutableList[TrainSet]) = {
    inputLayer ! GradientCheck(trainData, this)
    hiddenLayers.foreach(L => {
      L ! GradientCheck(trainData, this)
    })
  }

  /**
   * Forward propagation through the net
   * The Result is saved after each forward prop on the output layer we haven't it available for later use
   * @param trainData
   * @return
   */
  def forwardProp(trainData: mutable.MutableList[TrainSet]): Double = {
    var J = 0.0
    trainData.foreach(D => {
      input(D.x)
      Thread.sleep(10000)
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
  def backprop(trainData: mutable.MutableList[TrainSet]): Unit = {
    var counter = 1
    trainData.foreach(D => {
      if (counter % 100 == 0) {
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

    gradientChecking(trainData)

    inputLayer ! UpdateWeights(trainData.size, alpha)

    hiddenLayers.foreach(L => {
      L ! UpdateWeights(trainData.size, alpha)
    })

    J
  }

  def input(x: Array[Double]): Unit = {
    inputLayer ! ImpulsArray(x)
  }

  override def toString: String = {
    "Im a Neural Network Net \n" +
      "Input Layer: " + inputLayer.toString +
      "\n HiddenLayers: " + hiddenLayers.foreach(B => B)
    "Output Layer: " + outputLayer.toString
  }
}



