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
    neuronalNet.input(0, 0)
  }
}

class Net {
  def input(input1: Int, input2: Int): Unit = {
    // trigger bias with 0 value
    inputLayer.neurons.apply(0).setValue(0)
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
    neurons += new BiasNeuron
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
        neurons.foreach(C => {
          // cause im a bias 1*
          C.neuron.trigger(1*C.weight)
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
}




class OutputNeuron extends Neuron  {
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
        println(" Jepaa we finished forward Prop with result: " + result)
      }
      case ActivationWeight.reset => {
        resetValues()
        setValue(newValue)
      }
    }
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
          // trigger next connections
          neurons.foreach(C => {
            C.neuron.trigger(sigValue*C.weight)
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
}


class InputNeuron extends Neuron {

  override def trigger(newValue:Double, activationWeight: ActivationWeight): Unit = {
    neurons.foreach(N =>  {
      N.neuron.trigger(newValue*N.weight)
    })
  }

  override def setValue(value: Double): Unit = {
    this.value = value
    trigger(value)
  }
}

case class Connection(neuron: Neuron, weight:Double)


object ActivationWeight extends Enumeration {
  type ActivationWeight = Value
  val exeeded,reset, tolow = Value
}

abstract class Neuron {
  var value = 0.0
  var activationValue = 0.0
  var currentActivationValue = 0.0
  var neurons:mutable.MutableList[Connection] = mutable.MutableList[Connection]()



  def setValue(value:Double)

  def resetValues(): Unit = {
    this.value = 0
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
   * @param neuron
   * @return
   */
  def register(neuron:Neuron) = {
    neuron.increaseActivationValue()
    neurons += Connection(neuron, MathHelper.randomInitializedNumber())
  }

  override def toString():String = {
    "Neuron of: " + this.getClass + " ActivationValue: " + this.activationValue + " Current Activationvalue: " + this.currentActivationValue
  }
}

object MathHelper {
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
