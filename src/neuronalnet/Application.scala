package neuronalnet

import neuronalnet.ActivationWeight.ActivationWeight

import scala.collection.mutable
import scala.util.Random

/**
 * Created by Simon on 09.04.2015.
 */
object Application {
  def main(args: Array[String]) {
    val neuronalNet = new Net()

    var trainData = new mutable.MutableList[TrainSet]
    for (i <- 1 to 100) {
      trainData += TrainSet(0,0,0)
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
  }
}

case class TrainSet(x1:Int, x2:Int, y:Int)

class Net {

  def setResult(y: Int) = {
    outputLayer.neurons.apply(0).setResult(y)
  }

  def train(trainData: List[TrainSet]) = {
    var J = 0.0
    trainData.foreach(D => {
      input(D.x1, D.x2)
      val a_3 = outputLayer.getResult()
      val cost = -D.y*math.log(a_3) - (1 - D.y)*math.log(1 - a_3)
      J = J + cost
    })
    J = J / trainData.size

    val inputError = Array[Double](3)
    val hiddenError = Array[Double](2)
    trainData.foreach(D => {
      input(D.x1, D.x2)
      setResult(D.y)


      // Input Layer
      inputError(0) = inputError(0) + inputLayer.neurons.apply(0).getError()
      inputError(1) = inputError(1) + inputLayer.neurons.apply(1).getError()
      inputError(2) = inputError(2) + inputLayer.neurons.apply(2).getError()
      //

      // Hidden Layer
      hiddenError(0) = hiddenError(0) + hiddenLayer.neurons.apply(0).getError()
      hiddenError(1) = hiddenError(1) + hiddenLayer.neurons.apply(1).getError()

      //

    })

    // learning rate alpha
    val alpha = 0.9
    // new weights
    for (i <- 0 until inputError.length) {
      val newWeight = inputError(i)/trainData.size
      inputLayer.neurons.apply(i).postNeurons.apply(0).weight = inputLayer.neurons.apply(i).postNeurons.apply(0).weight - alpha*newWeight
    }
    for (i <- 0 until hiddenError.length) {
      val newWeight = hiddenError(i)/trainData.size

    }

    J = J / trainData.size





  }

  def input(input1: Int, input2: Int): Unit = {
    // trigger bias with 0 value
    inputLayer.neurons.apply(0).setValue(1)
    inputLayer.neurons.apply(1).setValue(input1)
    inputLayer.neurons.apply(2).setValue(input2)
  }

  var inputLayer = new InputLayer(2)
  var hiddenLayer = new HiddenLayer(1,inputLayer)
  var outputLayer = new OutputLayer(1,hiddenLayer)


  def createNet(): Unit = {

  }

  def forwardProp(value1:Int, value2:Int) = {

  }

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
  override def setValue(value: Double): Unit = {
    this.value = 1
    trigger(1)
  }

  override def trigger(newValue: Double, activationWeight: ActivationWeight): Unit = {
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
      }
    }
  }

  override def setResult(error: Double): Unit = {
    // bias unit has no delta but an error value to correct
    // TODO: Check this again really need an error @ bias neuron ?
    setError(error*1)

  }
}




class OutputNeuron extends Neuron  {
  var result:Double = 0


  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }

  override def trigger(newValue: Double,activationWeight: ActivationWeight): Unit = {
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
        println(" Jepaa we finished forward Prop with result: " + result)
      }
      case ActivationWeight.reset => {
        resetValues()
        setValue(newValue)
      }
    }
  }

  override def setResult(y: Double): Unit ={
    val delta = value - y
    preNeurons.foreach(C => {
      C.preNeuron.setResult(delta*C.weight)
    })
  }
}

class HiddenNeuron extends Neuron {


  override def trigger(newValue:Double,activationWeight: ActivationWeight): Unit = {
      // as soon as all connections pulled the trigger we can calculate forward
      // the first one resets the old value
      activationWeight match {
        case ActivationWeight.exeeded =>  {
          // sigmoid func
          increaseCurrentActivationValue()
          setValue(value + newValue)
          val sigValue = MathHelper.sigmoid(value)
          finalValue = sigValue
          // trigger next connections
          postNeurons.foreach(C => {
            C.postNeuron.trigger(sigValue*C.weight)
          })
        }
        case ActivationWeight.tolow => {
          setValue(value + newValue)
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
    setError(error*finalValue)

    val delta = error*MathHelper.sigmoidGradient(value)

    postNeurons.foreach(C => {
        C.preNeuron
    })
  }
}


class InputNeuron extends Neuron {

  override def trigger(newValue:Double, activationWeight: ActivationWeight): Unit = {
    postNeurons.foreach(N =>  {
      N.postNeuron.trigger(newValue*N.weight)
    })
  }

  override def setValue(value: Double): Unit = {
    this.value = value
    trigger(value)
  }

  override def setResult(error: Double): Unit = {
    setError(error*finalValue)
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
    "Neuron of: " + this.getClass + " ActivationValue: " + this.activationValue + " Current Activationvalue: " + this.currentActivationValue
  }
}

object MathHelper {
  def sigmoidGradient(z: Double) = {
    sigmoid(z) * (1 - sigmoid(z))
  }

  var random:Random = new Random()

  /*
  Sigmoid of z
   */
  def sigmoid(value:Double) : Double = {
    1.0 / (1.0 + math.exp(-value));
  }

  /*
  Gets a number [-epsiolon;epsilon]
   */
  def randomInitializedNumber():Double = {
    val epsilon = 0.12
    random.nextDouble()*2*epsilon - epsilon
  }
}
