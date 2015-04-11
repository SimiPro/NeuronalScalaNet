package neuronalnet

import neuronalnet.ActivationWeight.ActivationWeight

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

case class TrainSet(x1:Int, x2:Int, y:Int)

class Net {

  def setResult(y: Int) = {
    outputLayer.neurons.apply(0).setResult(y)
  }

  def train(trainData: mutable.MutableList[TrainSet]):Double = {
    var J = 0.0
    trainData.foreach(D => {
      input(D.x1, D.x2)
      val a_3 = outputLayer.getResult()
       var firstTirm = 0.0
      if (D.y == 0) {
        firstTirm = 0
      } else {
        firstTirm = -D.y*math.log(a_3)
      }
      // cant use this expression because he calculate every part of the form even if y = 0 just fail
      var cost = firstTirm - (1 - D.y)*math.log(1 - a_3)
      if (cost.isNaN) {
        cost = 0.0
      }
      J = J + cost
    })
    J = J / trainData.size


    val inputError = Array[Double](0.0,0.0,0.0)
    val hiddenError = Array[Double](0.0,0.0)
    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)

      // Hidden Layer
      val dt_2_0 = hiddenLayer.neurons.apply(0).getError()
      val dt_2_1 = hiddenLayer.neurons.apply(1).getError()


      hiddenError(0) = hiddenError(0) + dt_2_0
      hiddenError(1) = hiddenError(1) + dt_2_1

      //


      // Input Layer
      val dt_1_0 = inputLayer.neurons.apply(0).getError()
      val dt_1_1 = inputLayer.neurons.apply(1).getError()
      val dt_1_2 = inputLayer.neurons.apply(2).getError()
      inputError(0) = inputError(0) + dt_1_0
      inputError(1) = inputError(1) + dt_1_1
      inputError(2) = inputError(2) + dt_1_2
      //
    })
    // learning rate alpha
    val alpha = 0.9



    // new weights
    for (i <- 0 until inputError.length) {
      val newWeight = inputError(i)/trainData.size
      inputLayer.neurons.apply(i).postNeurons.apply(1).weight = inputLayer.neurons.apply(i).postNeurons.apply(1).weight - alpha*newWeight
    }
    for (i <- 0 until hiddenError.length) {
      val newWeight = hiddenError(i)/trainData.size
      hiddenLayer.neurons.apply(i).postNeurons.apply(0).weight = hiddenLayer.neurons.apply(i).postNeurons.apply(0).weight - alpha*newWeight
    }

    J
  }

  def input(input1: Int, input2: Int): Unit = {
    // trigger bias with value 1
    inputLayer.neurons.apply(0).setValue(1)
    inputLayer.neurons.apply(1).setValue(input1)
    inputLayer.neurons.apply(2).setValue(input2)
  }

  var inputLayer = new InputLayer(2)
  var hiddenLayer = new HiddenLayer(1,inputLayer)
  var outputLayer = new OutputLayer(1,hiddenLayer)
  /*
  inputLayer.neurons.apply(0).postNeurons.apply(1).weight = 6.9121
  inputLayer.neurons.apply(1).postNeurons.apply(1).weight = -4.9338
  inputLayer.neurons.apply(2).postNeurons.apply(1).weight = -4.9338

  hiddenLayer.neurons.apply(0).postNeurons.apply(0).weight = 5.07
  hiddenLayer.neurons.apply(1).postNeurons.apply(0).weight = -12.18
  */



}


abstract class Layer(units:Int) {
  var neurons:mutable.MutableList[Neuron] = mutable.MutableList[Neuron]()
  createUnits()

  def createUnits()


}

class InputLayer(units:Int) extends Layer(units) {
  override def createUnits(): Unit = {
    // add bias neuron first of all
    // in case of input layer this is a normal inputNeuron which we fill with 1 everytime
    neurons += new InputNeuron
    for (x <- 1 to units) {
      neurons += new InputNeuron
    }
  }
}

class HiddenLayer(units:Int, preLayer: Layer) extends Layer(units) {
  override def createUnits(): Unit = {
    neurons += new BiasNeuron
    for (x <- 1 to units) {
      neurons += new HiddenNeuron
    }
    /*
    create the connections to the previous neurons
     */
    neurons.foreach(N => {
      preLayer.neurons.foreach(preNeuron => {
        preNeuron.register(N)
      })
    })

  }
}
class OutputLayer(units:Int,preLayer: Layer) extends Layer(units) {

  def getResult():Double= {
      neurons.apply(0).value
  }


  override def createUnits(): Unit = {
    for (x <- 1 to units) {
      neurons += new OutputNeuron
    }

    neurons.foreach(N => {
      preLayer.neurons.foreach(preNeuron => {
        preNeuron.register(N)
      })
    })
  }
}

class BiasNeuron extends Neuron {
  this.value = 1

  override def setValue(value: Double): Unit = {
    this.value = 1
    trigger(1)
  }

  override def trigger(newValue: Double, activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)

    }

    activationWeight match {
      case ActivationWeight.exeeded => {
        increaseCurrentActivationValue()
        // trigger next connections
        postNeurons.foreach(C => {
          // cause im a bias 1*
          C.postNeuron.trigger(1*C.weight)
        })
      }
      case ActivationWeight.tolow => {
        increaseCurrentActivationValue()
      }
      case ActivationWeight.reset => {
        resetValues()
        value = 1
        increaseCurrentActivationValue()
      }
    }
  }

  override def setResult(error: Double): Unit = {
    // bias unit has no delta but an error value to correct
    // TODO: error = delta_3 * weight but we need just delta_3 change this
    setError(postNeurons.apply(0).postNeuron.getError()*1)
   // postNeurons.apply(0).weight = postNeurons.apply(0).weight - MathHelper.alpha*getError()

  }
}




class OutputNeuron extends Neuron  {
  var result:Double = 0


  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }

  override def trigger(newValue: Double,activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)
    }

    activationWeight match {
      case ActivationWeight.tolow => {
          increaseCurrentActivationValue()
          setValue(newValue)
      }
      case ActivationWeight.exeeded => {
        increaseCurrentActivationValue()
        setValue(newValue)
        val result = MathHelper.sigmoid(value)
        value = result
       // println(" Jepaa we finished forward Prop with result: " + result)
      }
      case ActivationWeight.reset => {
        resetValues()
        increaseCurrentActivationValue()
        setValue(newValue)
      }
    }
  }

  override def setResult(y: Double): Unit ={
    val delta = value - y
    setError(delta)

    preNeurons.foreach(C => {
      C.preNeuron.setResult(delta*C.weight)
    })
  }
}

class HiddenNeuron extends Neuron {


  override def trigger(newValue:Double,activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println(toString())
      println("New Value: " + newValue)
    }

      // as soon as all connections pulled the trigger we can calculate forward
      // the first one resets the old value
      activationWeight match {
        case ActivationWeight.exeeded =>  {
          // sigmoid func
          increaseCurrentActivationValue()
          setValue(newValue)
          val sigValue = MathHelper.sigmoid(value)
          finalValue = sigValue
          // trigger next connections
          postNeurons.foreach(C => {
            C.postNeuron.trigger(sigValue*C.weight)
          })
        }
        case ActivationWeight.tolow => {
          setValue(newValue)
          increaseCurrentActivationValue()
        }
        case ActivationWeight.reset => {
          resetValues()
          increaseCurrentActivationValue()
          setValue(newValue)
        }
      }
  }

  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }



  override def setResult(error: Double): Unit = {
    // set theta based on error from post neuron
    setError(postNeurons.apply(0).postNeuron.getError()*finalValue)

    // error = weight*delta_3

   // postNeurons.apply(0).weight = postNeurons.apply(0).weight - MathHelper.alpha*getError()
    val delta_2 = error*MathHelper.sigmoidGradient(value)

    preNeurons.foreach(C => {
      //TODO:  why no need of *C.weight ??? ? ??
        C.preNeuron.setResult(delta_2)
    })
  }
}


class InputNeuron extends Neuron {

  override def trigger(newValue:Double, activationWeight: ActivationWeight): Unit = {
    if (Application.DEBUG) {
      println("New Input: " + newValue)
    }
    postNeurons.foreach(N =>  {
      N.postNeuron.trigger(newValue*N.weight)
    })
  }

  override def setValue(value: Double): Unit = {
    this.value = value
    trigger(value)
  }

  override def setResult(error: Double): Unit = {
    setError(error*value)

 //   postNeurons.apply(0).weight = postNeurons.apply(0).weight - MathHelper.alpha*getError()

  }
}

case class Connection(postNeuron: Neuron, preNeuron:Neuron, var weight:Double)


object ActivationWeight extends Enumeration {
  type ActivationWeight = Value
  val exeeded,reset, tolow = Value
}

abstract class Neuron {
  def setResult(y: Double)

  var value = 0.0
  var finalValue = 0.0
  var activationValue = 0.0
  var currentActivationValue = 0.0
  var postNeurons:mutable.MutableList[Connection] = mutable.MutableList[Connection]()
  var preNeurons:mutable.MutableList[Connection] = mutable.MutableList[Connection]()
  var error = 0.0

  def setError(error: Double) = {
    this.error = error
  }

  def getError():Double= {
    return this.error
  }

  def setValue(value:Double)

  def resetValues(): Unit = {
    this.value = 0
    this.finalValue = 0
    this.currentActivationValue = 0
  }

  def increaseActivationValue(): Unit ={
    activationValue = activationValue + 1
  }

  def increaseCurrentActivationValue(): Unit = {
    currentActivationValue = currentActivationValue + 1
  }

  def trigger(newValue:Double): Unit = {
    // we need this special "reset" because we cant delete the weight after we used it for forward prop
    // because we need it for backprop too
    if (currentActivationValue == activationValue) {
        trigger(newValue, ActivationWeight.reset)
    } else if(currentActivationValue == activationValue - 1 ) {
        trigger(newValue, ActivationWeight.exeeded)
    } else {
        trigger(newValue, ActivationWeight.tolow)
    }
  }


  def trigger(newValue:Double, activationWeight: ActivationWeight)


  /**
   * it registers the neuron and set his activation weight +1
   * @param postNeuron
   * @return
   */
  def register(postNeuron:Neuron) = {
    postNeuron.increaseActivationValue()
    var connection = Connection(postNeuron, this, MathHelper.randomInitializedNumber())
    postNeurons += connection
    postNeuron.preNeurons += connection
  }

  override def toString():String = {
    "Neuron of: " + this.getClass +
    " \nActivationValue: " + this.activationValue +
    " \nCurrent Activationvalue: " + this.currentActivationValue +
    " \nActual Value: " + this.value
  }
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
