package neuronalnet.classification.neurons

import ActivationWeight.ActivationWeight
import neuronalnet.classification.math_.MathHelper

import scala.collection.mutable

/**
 * Created by Simon on 11.04.2015.
 */
abstract class Neuron {

  def setResult(y:Double): Unit = {
  }

  var value = 0.0
  var finalValue = 0.0
  var activationValue = 0.0
  var currentActivationValue = 0.0
  var postNeurons:mutable.MutableList[Connection] = mutable.MutableList[Connection]()
  var preNeurons:mutable.MutableList[Connection] = mutable.MutableList[Connection]()
  var error = 0.0
  var delta = 0.0

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
    this.error = 0.0
    this.delta = 0.0
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
