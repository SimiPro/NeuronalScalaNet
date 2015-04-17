package neuronalnet.classification.neurons

import _root_.akka.actor.ActorRef
import neuronalnet.classification.Application
import neuronalnet.classification.math_.MathHelper
import ActivationWeight.ActivationWeight
import neuronalnet.classification.neurons.akka._


/**
 * Created by Simon on 11.04.2015.
 */
class OutputNeuron(index:Int, outputLayer:ActorRef) extends Neuron(index) {
  var result: Double = 0


  override def setValue(value: Double): Unit = {
    this.value = this.value + value
  }

  override def trigger(newValue: Double, activationWeight: ActivationWeight): Unit = {
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
        value = MathHelper.sigmoid(value)
        outputLayer ! ResultImpuls(value, index)
      }
      case ActivationWeight.reset => {
        resetValues()
        increaseCurrentActivationValue()
        setValue(newValue)
      }
    }
  }


  override def setResult(y: Double): Unit = {
    val delta = (value - y)

    preNeurons.foreach(C => {
      C ! DeltaImpuls(delta)
    })
  }
}
